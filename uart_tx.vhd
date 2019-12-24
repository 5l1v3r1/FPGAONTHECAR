library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;


entity uart_tx is 
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
	end uart_tx;
	
architecture Behavioral of uart_tx is
	constant Clk_bit : integer := clk_freq / baudrate + 1;
	
	type t_uart_tx is (IDLE,START,SEND,FINISH,DONE);
	signal r_uart_tx : t_uart_tx := IDLE;
	signal r_clk_cnt : integer range 0 to clk_bit - 1 := 0;
	signal r_data_ind : integer range 0 to 7 := 0;
	signal r_data : std_logic_vector(7 downto 0) := (others => '0');
	signal r_tx: std_logic := '1';
	signal r_tx_done : std_logic := '0';
	
	begin
	
	out_tx <= r_tx;
	out_tx_done <= r_tx_done;
	
	
	process(in_clk, in_rst)
	begin
	
	
	if in_rst = '1' then
		r_uart_tx <= IDLE;
		r_clk_cnt <= 0;
		r_data_ind <= 0;
		r_data <= (others => '0');
		r_tx <= '1';
		r_tx_done <= '0';
		
	elsif rising_edge(in_clk) then
		r_tx_done <= '0';
		
		case r_uart_tx is 
			when IDLE => 
				r_tx <= '1';
				r_clk_cnt <= 0;
				r_data_ind <= 0;
				if in_tx_start = '1' then 
					r_data <= in_tx_data;
					r_uart_tx <= START;
				else
					r_clk_cnt <= r_clk_cnt + 1;
				end if;
			when SEND => 
				r_tx <= '0';
				if r_clk_cnt = CLK_BIT - 1 then
					r_clk_cnt <= 0;
					if r_data_ind = 7 then 
						r_data_ind <= 0;
						r_uart_tx <= FINISH;
					else
						r_data_ind <= r_data_ind + 1;
					end if;
				else
					r_clk_cnt <= r_clk_cnt + 1;
				end if;
			when FINISH =>
				r_tx <= '1';
				if r_clk_cnt = CLK_BIT - 1 then
					r_clk_cnt <= 0;
					r_uart_tx <= DONE;
				else
					r_clk_cnt <= r_clk_cnt + 1;
				end if;
			when DONE =>
				r_tx <= '1';
				r_tx_done <= '1';
				r_uart_tx <= IDLE;
			when others => NULL;
		end case;
	end if;
end process;
	
end Behavioral;