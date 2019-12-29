library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity main is 
	PORT(
		in_clk: in std_logic;
		in_rst: in std_logic;
		in_rx : in std_logic;
		out_tx : out std_logic;
		led1 : out std_logic
	);
	end main;
	
architecture Behavioral of main is
    
    -- Components

	component uart_tx
	Generic(
		CLK_FREQ : integer := 12000000;
		BAUDRATE: integer := 9600
	);
	PORT(
		in_clk: in std_logic;
		in_rst: in std_logic;
		in_tx_start: in std_logic;
		in_tx_data: in std_logic_vector(7 downto 0);
		sda : inout STD_LOGIC;
		scl : inout STD_LOGIC;
		out_tx: out std_logic;
		out_tx_done: out std_logic
	);
	end component;
	
	component uart_rx
	Generic(
		CLK_FREQ : integer := 12000000;
		BAUDRATE: integer := 9600
	);
	PORT(
		in_clk: in std_logic;
		in_rst: in std_logic;
		in_rx : in std_logic;
		out_rx_data : out std_logic_vector(7 downto 0);
		out_rx_done : out std_logic
	);
	end component;
	
	component fifo is
        generic (
          g_WIDTH : natural := 8;
          g_DEPTH : integer := 32
          );
        port (
          i_rst_sync : in std_logic;
          i_clk      : in std_logic;
     
          -- FIFO Write Interface
          i_wr_en   : in  std_logic;
          i_wr_data : in  std_logic_vector(g_WIDTH-1 downto 0);
          o_full    : out std_logic;
     
          -- FIFO Read Interface
          i_rd_en   : in  std_logic;
          o_rd_data : out std_logic_vector(g_WIDTH-1 downto 0);
          o_empty   : out std_logic
          );
      end component fifo;
      
    -- I2C Master
    component i2c_master is
             GENERIC(
               input_clk : INTEGER := 100_000_000; --input clock speed from user logic in Hz
               bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
             PORT(
                 clk       : IN     STD_LOGIC;                    --system clock
                 reset_n   : IN     STD_LOGIC;                    --active low reset
                 ena       : IN     STD_LOGIC;                    --latch in command
                 addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
                 rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
                 data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
                 busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
                 data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
                 ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
                 sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
                 scl       : INOUT  STD_LOGIC);     
    end component i2c_master;
    -- DC Motor
    
    component dc_motor is 
        port ( 
            clk : in std_logic;   
            kontak : in std_logic;            
            ileri : in std_logic;           
            geri : in std_logic;           
            sag : in std_logic;           
            sol : in std_logic;        
            enable : out std_logic;       
            output1 : out std_logic;           
            output2 : out std_logic;           
            enable2 : out std_logic);
	end component dc_motor;
	
	
	-- types that are related to components and all system
	type t_data_cntrl is (IDLE,REC,SEND);
	type vehicle is (START,IDLE,GOING,ONLOC);
	
	
	-- Signal that should be connected to components
	-- UART
	signal r_data_cntrl : t_data_cntrl := IDLE;
	signal r_tx_start : std_logic := '0';
	signal r_tx_done : std_logic := '0';
	signal r_rx_done : std_logic := '0';
	signal r_data : std_logic_vector(7 downto 0);
	signal r_rx_data : std_logic_vector(7 downto 0);
	signal r_tx_data : std_logic_vector(7 downto 0);
	
	--FIFO
	constant c_DEPTH : integer := 8;
    constant c_WIDTH : integer := 11;
       
    signal r_WR_EN   : std_logic := '0';
    signal r_WR_DATA : std_logic_vector(c_WIDTH-1 downto 0) := r_rx_data;
    signal w_FULL    : std_logic;
    signal r_RD_EN   : std_logic := '0';
    signal w_RD_DATA : std_logic_vector(c_WIDTH-1 downto 0);
    signal w_EMPTY   : std_logic;
    
    -- I2C 
    constant input_clk : INTEGER := 100_000_000;
    constant bus_clk : integer := 400_000;
           
    signal ena   : std_logic := '0';
    signal reset_n  : std_logic := '0';
    signal rw : std_logic := '0';
    signal addr : std_logic_vector(7 downto 0) := "111011X"; -- x should be changed from the datasheet value
    signal data_wr: std_logic_vector(7 DOWNTO 0) ;
    signal busy  : std_logic := '0';                    
    signal data_rd : STD_LOGIC_VECTOR(7 DOWNTO 0); 
    signal ack_error : STD_LOGIC;                   
 
    
	
	-- Signals that should be connected to main design
	
	
	
	--
	
	begin
	
	led1 <= in_rx;
	
	
	uart_tx_map :  uart_tx
        Generic map(
            CLK_FREQ => 12000000,
            BAUDRATE => 9600
        )
        PORT map(
            in_clk => in_clk,
            in_rst => in_rst,
            in_tx_start => r_tx_start,
            in_tx_data => r_rx_data,
            out_tx => out_tx,
            out_tx_done => r_tx_done
        );
        
        
        uart_rx_map :  uart_rx
        Generic map (
            CLK_FREQ => 12000000,
            BAUDRATE=> 9600
        )
        PORT map(
            in_clk => in_clk,
            in_rst => in_rst,
            in_rx => in_rx,
            out_rx_data => r_rx_data,
            out_rx_done => r_rx_done
        );
        FIFO_MAP : fifo
            generic map (
              g_WIDTH => c_WIDTH,
              g_DEPTH => c_DEPTH
              )
            port map (
              i_rst_sync => in_rst,
              i_clk      => in_clk,
              i_wr_en    => r_WR_EN,
              i_wr_data  => r_WR_DATA,
              o_full     => w_FULL,
              i_rd_en    => r_RD_EN,
              o_rd_data  => w_RD_DATA,
              o_empty    => w_EMPTY
              );
              
         I2C_MAP: i2c_master
                     generic map (
                       input_clk => input_clk,
                       bus_clk => bus_clk
                       )
                     port map (
                       clk => in_clk ,
                       reset_n => in_rst,
                       ena  => ena ,
                       addr  => addr,
                       rw => rw,
                       data_wr => data_wr,
                       busy  => busy,
                       data_rd    => data_rd,
                       ack_error  => ack_error,
                       sda    => w_EMPTY,
                       scl    => w_EMPTY
                       );
                       
         DC_MAP: dc_motor  
             port map ( 
                       clk => in_clk ,
                       kontak => in_clk ,           
                       ileri => in_clk ,          
                       geri => in_clk ,           
                       sag => in_clk ,           
                       sol => in_clk ,     
                       enable => in_clk ,       
                       output1 => in_clk ,           
                       output2 => in_clk ,            
                       enable2 => in_clk );
 
	
	process(in_clk, in_rst)
	begin
		if in_rst = '1' then
			r_data_cntrl <= IDLE;
			r_data <= (others => '0');
		elsif rising_edge(in_clk) then
			r_tx_start <= '0';
			
			case r_data_cntrl is
				when IDLE =>
					r_data_cntrl <= REC;
				when REC =>
					if r_rx_done = '1' then
						r_tx_data <= r_rx_data;
						r_tx_start <= '1';
					else
					   r_WR_EN <= '1';
					   r_data_cntrl <= SEND;
					end if;
				when SEND => 
				    r_WR_EN <= '0';
					if r_tx_done = '1' then
						r_data_cntrl <= IDLE;
					end if;
				when others => NULL;
			end case;
		end if;
	end process;
	
	
	
	
	




end Behavioral;
