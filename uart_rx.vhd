library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;


entity uart_rx is 
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
	end uart_rx;
	
architecture Behavioral of uart_rx is

	constant Clk_bit : integer := clk_freq / baudrate + 1;

	type t_uart_rx is (IDLE,START,REC,FINISH,DONE);
	signal r_uart_rx : t_uart_rx := IDLE;
	signal r_clk_cnt : integer range 0 to Clk_bit - 1 := 0;
	signal r_data_ind : integer range 0 to 7 := 0;
	signal r_data : std_logic_vector(7 downto 0) := (others => '0');
	signal r_rx_done : std_logic := '0';
	signal r_rx_cnt : std_logic_vector(2 downto 0) := (others => '0');
	
	begin
	
		out_rx_data <= r_data;
		out_rx_done <= r_rx_done;
		
		process(in_clk)
		begin
			if in_rst = '1' then
				r_UART_rx <= IDLE;
				r_clk_cnt <= 0;
				r_data_ind <= 0;
				r_data <= (others => '0');
				r_rx_cnt <= (others => '0');
				r_rx_done <= '0';
			elsif rising_edge(in_clk) then
				r_rx_cnt <= r_rx_cnt(1 downto 0) & in_rx;
				r_rx_done <= '0';
				
				case r_uart_rx is
					when IDLE =>
						if r_rx_cnt(2 downto 1) = "10" then
							r_uart_rx <= START;
						end if;
					when START =>
						if r_clk_cnt = (CLK_BIT - 1 )/2 then
							r_clk_cnt <= 0;
							r_uart_rx <= REC;
						else 
							r_clk_cnt <= r_clk_cnt + 1;
						end if;
					when REC =>
						r_data(r_data_ind) <= r_rx_cnt(2);
						
						if r_clk_cnt = CLK_BIT - 1 then 
							r_clk_cnt <= 0;
							if r_data_ind = 7 then
								r_data_ind <= 0;
								r_uart_rx <= FINISH;
							else
								r_data_ind <= r_data_ind + 1;
							end if;
						else
							r_clk_cnt <= r_clk_cnt + 1;
						end if;
					when FINISH =>
						if r_clk_cnt = CLK_BIT - 1 then
							r_clk_cnt <= 0;
							r_uart_rx <= DONE;
						else
							r_clk_cnt <= r_clk_cnt + 1;
						end if;
					
					when DONE =>
						r_rx_done <= '1';
						r_uart_rx <= IDLE;
					when others => NULL;
				end case;
			end if;
		end process;
end Behavioral;