library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
entity i2c_leds is
  generic(
    clk_freq : integer := 50_000_000;
    resolution : integer := 5; --desired resolution of temperature data in bits
    temp_sensor_address: std_logic_vector (6 downto 0) := "1110110"); --I2C address of the temp sensor pmod
 
  port (
    scl              : inout std_logic;
    sda              : inout std_logic;
    clk              : in    std_logic;
    reset_n          : in    std_logic; 
    i2c_ack_err      : BUFFER std_logic 
    
  );
end entity;

architecture Behavioral of i2c_leds is
    type machine is (start, set_resolution, set_reg_pointer, read_data, output_result);
    
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
    
    
    component i2c_master is
        generic (
        input_clk: integer ; --input clock speed from user logic (Hz)
        bus_clk : integer ); --speed the i2c bus will run 
        port (
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
    end component ; 
--CelsiusTemp = (double(dataCaptureOut.temperature))/(2^4)*503.975/4096 - 273.15;
--sprintf('The FPGA Temperature is %fC\n',mean(CelsiusTemp))
begin
--instantiate the i2c master
i2c_master_0: i2c_master
    generic map(
    input_clk => clk_freq, 
    bus_clk=> 400_000)
    port map (
    clk => clk,
    reset_n => reset_n,
    ena => i2c_ena ,
    addr => i2c_address,
    rw => i2c_rw,
    data_wr => i2c_data_wr,
    busy => i2c_busy ,
    data_rd => i2c_data_rd,
    ack_error => i2c_ack_err,
    sda => sda,
    scl => scl);
    
  process(clk,reset_n)
  variable busy_cnt : integer range 2 downto 0 :=0 ;
  variable counter : integer range clk_freq/10 downto 0 := 0;
  begin
    if(reset_n = '0') then 
    --Clear all values 
        counter := 0;
        i2c_ena <= '0';
        busy_cnt := 0;
        temperature <= (others => '0'); --clear temp result
        state <= start;
    elsif (clk 'event and clk = '1') then 
        case state is
            when start =>
             if(counter < clk_freq/10) then 
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
end architecture;