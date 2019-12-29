 library IEEE;

use IEEE.STD_LOGIC_1164.ALL;

use IEEE.STD_LOGIC_ARITH.ALL;

use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity dc_motor is 
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
end dc_motor;

architecture Behavioral of dc_motor is 
begin 
process(kontak)
--variable i : integer := 1;


begin

if kontak = '1'  then

	

		 enable <= '0';
		 enable2 <= '0';

			if ileri = '1' --i <= 1005 
				then --i := i + 1;

				enable <= '1';
				enable2 <= '1';
				output1 <= '1';

				output2 <= '0';
				
			elsif geri = '1' --i > 1005 and i < 1550
				then 
				
				enable <= '1';
				enable2 <= '1';
				output1 <= '0';

				output2 <= '1';
				
			elsif sol = '1'
				then --i := i + 1;
				
				enable <= '0';
				enable2 <= '1';
				output1 <= '0';

				output2 <= '1';
				
			elsif sag = '1'
				then --i := i + 1;
				
				enable <= '0';
				enable2 <= '1';
				output1 <= '1';

				output2 <= '0';
				
			--else  -- i = 1550 then i := 0;

			end if;

else enable <= '0';
		 enable2 <= '0';
end if; 
					
end process;

end Behavioral;