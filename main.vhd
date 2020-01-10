library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity main is 
	PORT(
		in_clk: in std_logic;
		in_rst: in std_logic;
		in_rx : in std_logic;
		out_tx : out std_logic;
		sda: inout std_logic;
		scl: inout std_logic;
		led1 : out std_logic;
		i2c_ack_err      : BUFFER std_logic 
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
        generic (
        input_in_clk: integer ; --input clock speed from user logic (Hz)
        bus_in_clk : integer ); --speed the i2c bus will run 
        port (
        in_clk       : IN     STD_LOGIC;                    --system clock
        in_rst   : IN     STD_LOGIC;                    --active low reset
        ena       : IN     STD_LOGIC;                    --latch in command
        addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
        rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
        data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
        busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
        data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
        ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
        sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
        scl       : INOUT  STD_LOGIC); 
    end component ; 
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
	
	
	-- General States
	signal main_cntrl : vehicle := IDLE;
	
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
	constant c_DEPTH : integer := 32;
    constant c_WIDTH : integer := 8;
       
    signal r_WR_EN   : std_logic := '0';
    signal r_WR_DATA : std_logic_vector(c_WIDTH-1 downto 0);
    signal w_FULL    : std_logic;
    signal r_RD_EN   : std_logic := '0';
    signal w_RD_DATA : std_logic_vector(c_WIDTH-1 downto 0);
    signal w_EMPTY   : std_logic;
    
    -- I2C 
    signal state : machine ; --state machine
    signal config : std_logic_vector(7 downto 0); --value to set the bme280 configuration
    signal i2c_ena : std_logic ;
    signal i2c_address : std_logic_vector (6 downto 0);
    signal i2c_rw : std_logic ;
    signal i2c_data_wr : std_logic_vector (7 downto 0);
    signal i2c_data_rd : std_logic_vector (7 downto 0);
    signal i2c_busy : std_logic ;
    signal busy_prev : std_logic ; --previous value of i2c busy signal
    signal temp_data  :std_logic_vector (15 downto 0); --temperature data buffer 
    signal temperature :  std_logic_vector(resolution-1 downto 0);
    signal senddata : std_logic_vector(10 downto 0);                 
 
    
    
    -- DC MOTOR
    signal kontak : std_logic := '0';           
    signal ileri  : std_logic := '0';                
    signal geri  : std_logic := '0';                      
    signal sag : std_logic := '0';        
    signal sol : std_logic := '0';         
    signal enable : std_logic := '0';       
    signal output1 : std_logic := '0';           
    signal output2 : std_logic := '0';                 
    signal enable2 : std_logic := '0';       
    
	
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
              
      i2c_master_0: i2c_master
    generic map(
    input_in_clk => in_clk_freq, 
    bus_in_clk=> 400_000)
    port map (
    in_clk => in_clk,
    in_rst => in_rst,
    ena => i2c_ena ,
    addr => i2c_address,
    rw => i2c_rw,
    data_wr => i2c_data_wr,
    busy => i2c_busy ,
    data_rd => i2c_data_rd,
    ack_error => i2c_ack_err,
    sda => sda,
    scl => scl);
                       
         DC_MAP: dc_motor  
             port map ( 
                       clk => in_clk ,
                       kontak => kontak ,           
                       ileri => ileri ,          
                       geri => geri ,           
                       sag => sag ,           
                       sol => sol ,     
                       enable => enable ,       
                       output1 => output1 ,           
                       output2 => output2 ,            
                       enable2 => enable2 );
 
	
	mainproc: process(in_clk, in_rst)
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

						r_WR_DATA <= r_rx_data;
						r_tx_start <= '1';
					else
					   r_RD_EN <= '0';
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
	
	
	process(in_clk, in_rst)
	   
        	   
        begin
           if in_rst = '1' then
                    main_cntrl <= IDLE;
                   
           elsif rising_edge(in_clk) then
               case main_cntrl is 
               
               when IDLE => 
                -- 0. GO
                -- 1. BASE
                -- 2-4. COORD X
                -- 5-7. COORD Y 
                    r_RD_EN <= '1';
                    if w_RD_DATA /= "00000000" then
                        main_cntrl <= START;
                    end if;
                when START =>
                    r_RD_EN <= '0';
                    
                    
                when GOING => 
                    r_RD_EN <= '0';
                    
                    
                    
                when ONLOC =>
                    r_RD_EN <= '0';
                    
               
                when others => NULL;
               end case;
           end if;
           
           
    end process;
	
    i2cproc:process(in_clk,in_rst)
  variable busy_cnt : integer range 2 downto 0 :=0 ;
  variable counter : integer range in_clk_freq/10 downto 0 := 0;
  begin
    if(in_rst = '0') then 
    --Clear all values 
        counter := 0;
        i2c_ena <= '0';
        busy_cnt := 0;
        temperature <= (others => '0'); --clear temp result
        state <= start;
    elsif (in_clk 'event and in_clk = '1') then 
        case state is
            when start =>
             if(counter < in_clk_freq/10) then 
                counter := counter+1 ;
             else 
                counter := 0;
                state<=set_resolution;
             end if;
             
             when set_resolution => 
                busy_prev <= i2c_busy;
                if (busy_prev = '0' and i2c_busy ='1') then
                    busy_cnt := busy_cnt+1;
                end if;
                
                case busy_cnt is
                    when 0 => 
                        i2c_ena <='1';
                        i2c_address <= temp_sensor_address;
                        i2c_rw <='0';
                        i2c_data_wr <= "00000001";
                    when 1 =>
                        i2c_data_wr <= config ;
                    when 2 => 
                        i2c_ena <= '0';                              --deassert enable to stop transaction after command 2
              IF(i2c_busy = '0') THEN                      --transaction complete
                busy_cnt := 0;                               --reset busy_cnt for next transaction
                state <= set_reg_pointer;                    --advance to setting the Register Pointer for data reads
              END IF;
            WHEN OTHERS => NULL;
          END CASE;
          
        --set the register pointer to the Ambient Temperature Register  
        WHEN set_reg_pointer =>
          busy_prev <= i2c_busy;                       --capture the value of the previous i2c busy signal
          IF(busy_prev = '0' AND i2c_busy = '1') THEN  --i2c busy just went high
            busy_cnt := busy_cnt + 1;                    --counts the times busy has gone from low to high during transaction
          END IF;
          CASE busy_cnt IS                             --busy_cnt keeps track of which command we are on
            WHEN 0 =>                                    --no command latched in yet
              i2c_ena <= '1';                              --initiate the transaction
              i2c_address <= temp_sensor_address;                --set the address of the temp sensor
              i2c_rw <= '0';                               --command 1 is a write
              i2c_data_wr <= "00000000";                   --set the Register Pointer to the Ambient Temperature Register
            WHEN 1 =>                                    --1st busy high: command 1 latched
              i2c_ena <= '0';                              --deassert enable to stop transaction after command 1
              IF(i2c_busy = '0') THEN                      --transaction complete
                busy_cnt := 0;                               --reset busy_cnt for next transaction
                state <= read_data;                          --advance to reading the data
              END IF;
            WHEN OTHERS => NULL;
          END CASE;
        
        --read ambient temperature data
        WHEN read_data =>
          busy_prev <= i2c_busy;                       --capture the value of the previous i2c busy signal
          IF(busy_prev = '0' AND i2c_busy = '1') THEN  --i2c busy just went high
            busy_cnt := busy_cnt + 1;                    --counts the times busy has gone from low to high during transaction
          END IF;
          CASE busy_cnt IS                             --busy_cnt keeps track of which command we are on
            WHEN 0 =>                                    --no command latched in yet
              i2c_ena <= '1';                              --initiate the transaction
              i2c_address <= temp_sensor_address;                --set the address of the temp sensor
              i2c_rw <= '1';                               --command 1 is a read
            WHEN 1 =>                                    --1st busy high: command 1 latched, okay to issue command 2
              IF(i2c_busy = '0') THEN                      --indicates data read in command 1 is ready
                temp_data(15 DOWNTO 8) <= i2c_data_rd;       --retrieve MSB data from command 1
              END IF;
            WHEN 2 =>                                    --2nd busy high: command 2 latched
              i2c_ena <= '0';                              --deassert enable to stop transaction after command 2
              IF(i2c_busy = '0') THEN                      --indicates data read in command 2 is ready
                temp_data(7 DOWNTO 0) <= i2c_data_rd;        --retrieve LSB data from command 2
                busy_cnt := 0;                               --reset busy_cnt for next transaction
                state <= output_result;                      --advance to output the result
              END IF;
           WHEN OTHERS => NULL;
          END CASE;

        --output the temperature data
        WHEN output_result =>
          temperature <= temp_data(15 DOWNTO 16-resolution);  --write temperature data to output
          state <= read_data;                                 --retrieve the next temperature data

        --default to start state
        WHEN OTHERS =>
          state <= start;

      END CASE;
    END IF;
          
             
        
  end process;
	
	




end Behavioral;
