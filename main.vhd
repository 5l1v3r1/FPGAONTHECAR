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
	
	type t_data_cntrl is (IDLE,REC,SEND);
	signal r_data_cntrl : t_data_cntrl := IDLE;
	signal r_tx_start : std_logic := '0';
	signal r_tx_done : std_logic := '0';
	signal r_rx_done : std_logic := '0';
	signal r_data : std_logic_vector(7 downto 0);
	signal r_rx_data : std_logic_vector(7 downto 0);
	signal r_tx_data : std_logic_vector(7 downto 0);
	
	
	begin
	
	led1 <= in_rx;
	
	
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
					end if;
				when SEND => 
					if r_tx_done = '1' then
						r_data_cntrl <= IDLE;
					end if;
				when others => NULL;
			end case;
		end if;
	end process;
	
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
	




end Behavioral;