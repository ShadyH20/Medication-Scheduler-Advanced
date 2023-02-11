LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
USE work.clk_div_pkg.ALL;
USE work.debounce_pkg.ALL;
--use work.gate_pkg.all;

ENTITY project1 IS
    PORT (
        clk : IN STD_LOGIC; -- clock 50 MHz
        rst_n : IN STD_LOGIC;

        -- 7-segment outputs
        H_out1 : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
        H_out0 : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
        M_out1 : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
        M_out0 : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
        S_out1 : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
        S_out0 : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);

        sensor : IN STD_LOGIC;
        buzzer : BUFFER STD_LOGIC;
        servo : BUFFER STD_LOGIC;

        -- Inputs for time editing and setting
        time_input : IN STD_LOGIC;
        alarm_input : IN STD_LOGIC;
        digit_inc : IN STD_LOGIC;
        digit_confirm : IN STD_LOGIC;

        led1 : OUT STD_LOGIC;
        led2 : OUT STD_LOGIC;
--		  digit: OUT STD_LOGIC_VECTOR(3 downto 0);
        alarm_setO : OUT STD_LOGIC

    );
END project1;

ARCHITECTURE Behavioral OF project1 IS
    COMPONENT bin2hex
        PORT (
            Bin : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            Hout : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT clk_div
        PORT (
            CLK50MHZ : IN STD_LOGIC;
            --    CPU_RESETN: in std_logic;
            --    reset: in std_logic;
            clk_stb : BUFFER STD_LOGIC
        );
    END COMPONENT;
    COMPONENT int2bin IS
        PORT (
			  clk, tin, ain, i : IN STD_LOGIC;
			  myDig, currDig : IN INTEGER;
			  counter, temp : IN INTEGER;
			  bin_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
    );
    END COMPONENT;
	 
	 function toBin1(num:INTEGER)
		return STD_LOGIC_VECTOR is
	 BEGIN
			IF num < 10 THEN
				return  x"0";
			ELSIF num < 20 THEN
				return x"1";
			ELSIF num < 30 THEN
				return  x"2";
			ELSIF num < 40 THEN
				return x"3";
			ELSIF num < 50 THEN
				return  x"4";
			ELSE
				return x"5";	
			END IF;
	 END function toBin1;
	 
	 function toBin0(num:INTEGER;tens:STD_LOGIC_VECTOR)
		return STD_LOGIC_VECTOR is
	 BEGIN
			return STD_LOGIC_VECTOR(to_unsigned((num - to_integer(unsigned(tens)) * 10), 4));
	 END function toBin0;

    -- Values for reset
    SIGNAL H_in1 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL H_in0 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL M_in1 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL M_in0 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL S_in1 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL S_in0 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";

    SIGNAL clk_1s : STD_LOGIC; -- 1-s clock
    SIGNAL counter_hour, counter_minute, counter_second : INTEGER;

    SIGNAL alarm_hour, alarm_minute, alarm_second : INTEGER; -- for alarm time

    SIGNAL temp_hour, temp_minute, temp_second : INTEGER := 0; -- for time and alarm setting
	 SIGNAL tempH1 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL tempH0 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL tempM1 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL tempM0 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL tempS1 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";
    SIGNAL tempS0 : STD_LOGIC_VECTOR(3 DOWNTO 0) := x"0";

    -- counter used for create time
    SIGNAL H_out1_bin : STD_LOGIC_VECTOR(3 DOWNTO 0); --The most significant digit of the hour
    SIGNAL H_out0_bin : STD_LOGIC_VECTOR(3 DOWNTO 0);--The least significant digit of the hour
    SIGNAL M_out1_bin : STD_LOGIC_VECTOR(3 DOWNTO 0);--The most significant digit of the minute
    SIGNAL M_out0_bin : STD_LOGIC_VECTOR(3 DOWNTO 0);--The least significant digit of the minute
    SIGNAL S_out1_bin : STD_LOGIC_VECTOR(3 DOWNTO 0);--The most significant digit of the second
    SIGNAL S_out0_bin : STD_LOGIC_VECTOR(3 DOWNTO 0);--The least significant digit of the second

    SIGNAL buzzCounter : INTEGER RANGE 0 TO 10;
    SIGNAL digit_n : INTEGER RANGE 0 TO 5 := 0;
	 SIGNAL time_input_prev : STD_LOGIC;
	 SIGNAL inc : STD_LOGIC;
	 SIGNAL confirm : STD_LOGIC;
	 SIGNAL alarm_set : STD_LOGIC := '0';
	 SIGNAL can_inc : BOOLEAN;
	 SIGNAL can_confirm : BOOLEAN;
	 SIGNAL can_set_time : BOOLEAN := FALSE;
	 SIGNAL can_set_alarm : BOOLEAN := FALSE;
	 

BEGIN

	 deb0: debounce PORT MAP (clk, '1', digit_inc, inc);
	 deb1: debounce PORT MAP (clk, '1', digit_confirm, confirm);
	 
--	 digit <= STD_LOGIC_VECTOR(to_unsigned(digit_n, 4));

    ---SENSOR AND BUZZER---
    --	servoAssign:  entity work.gate(main) port map(clk, sensor, servo);

    -- create 1-s clock --|
    create_1s_clock : clk_div PORT MAP(clk, clk_1s);
	 
	 PROCESS (clk) BEGIN
			alarm_setO <= alarm_set;
	 END PROCESS;


    -- clock operation ---|
    PROCESS (clk_1s, rst_n) BEGIN

		  time_input_prev <= time_input;
			
        -- RESET --
        IF (rst_n = '0') THEN
            counter_hour <= to_integer(unsigned(H_in1)) * 10 + to_integer(unsigned(H_in0));
            counter_minute <= to_integer(unsigned(M_in1)) * 10 + to_integer(unsigned(M_in0));
            counter_second <= to_integer(unsigned(S_in1)) * 10 + to_integer(unsigned(S_in0));

		  -- SETTING THE TIME --
		  ELSIF time_input = '0' AND can_set_time THEN 	-- FALLING EDGE
				can_set_time <= FALSE;
				counter_hour   <= to_integer(unsigned(tempH1)) * 10 + to_integer(unsigned(tempH0));
				counter_minute <= to_integer(unsigned(tempM1)) * 10 + to_integer(unsigned(tempM0));
				counter_second <= to_integer(unsigned(tempS1)) * 10 + to_integer(unsigned(tempS0));
			
		  -- SETTING THE ALARM --
		  ELSIF alarm_input = '0' AND can_set_alarm THEN 	-- FALLING EDGE
				can_set_alarm <= FALSE;
				alarm_set <= '1';
				alarm_hour   <= to_integer(unsigned(tempH1)) * 10 + to_integer(unsigned(tempH0));
				alarm_minute <= to_integer(unsigned(tempM1)) * 10 + to_integer(unsigned(tempM0));
				alarm_second <= to_integer(unsigned(tempS1)) * 10 + to_integer(unsigned(tempS0));
            -- EVERY 1s --		
        ELSIF (rising_edge(clk_1s)) THEN

            ---- SENSOR CHECK ----
            IF sensor = '0' AND buzzer = '1' THEN
                servo <= '1';
                buzzer <= '0';
                buzzCounter <= 0;
            END IF;

            IF (servo = '1') THEN
                buzzCounter <= buzzCounter + 1;
            END IF;

            IF (buzzCounter >= 5) THEN
                servo <= '0';
                buzzCounter <= 0;
            END IF;
            ---------------------

            counter_second <= counter_second + 1;
				
				-- RING WHEN ALARM TIME COMES --
            IF (alarm_set = '1' AND
					counter_hour = alarm_hour AND counter_minute = alarm_minute AND counter_second = alarm_second) THEN
                buzzer <= '1';
            END IF;

            IF (counter_second >= 59) THEN -- second > 59 then minute increases
                counter_minute <= counter_minute + 1;
                counter_second <= 0;
                IF (counter_minute >= 59) THEN -- minute > 59 then hour increases
                    counter_minute <= 0;
                    counter_hour <= counter_hour + 1;
                    IF (counter_hour >= 24) THEN -- hour > 24 then set hour to 0
                        counter_hour <= 0;
                    END IF;
                END IF;
            END IF;
        END IF;
		  
		  ----------------------
		  ---- TIME SETTING ----
		  ----------------------
		  -- If both ON, time input takes precedence
        led1 <= time_input;
        IF time_input = '1' THEN
            led2 <= '0';
				can_set_time <= TRUE;
        ELSE
            led2 <= alarm_input;
        END IF;
		  
		  IF alarm_input = '1' THEN
			   can_set_alarm <= TRUE;
		  END IF;
		  
		  

    END PROCESS;

	 PROCESS (inc, confirm)
	 BEGIN
			IF(time_input = '1' OR alarm_input = '1') THEN
				IF (FALLING_EDGE(confirm)) THEN
					digit_n <= digit_n + 1;	
				END IF;
				
				IF (FALLING_EDGE(inc)) THEN
					CASE digit_n IS
						WHEN 0 =>
							IF tempH1 < x"2" THEN
								tempH1 <= STD_LOGIC_VECTOR(to_unsigned((to_integer(unsigned(tempH1)) + 1), 4));
							ELSE 
								tempH1 <= x"0";
							END IF;
							
						WHEN 1 =>
							IF (tempH1 = x"2" AND tempH0 < x"3") OR (tempH1 < x"2" AND tempH0 < x"9") THEN
								tempH0 <= STD_LOGIC_VECTOR(to_unsigned((to_integer(unsigned(tempH0)) + 1), 4));
							ELSE
								tempH0 <= x"0";
							END IF;
							
						WHEN 2 =>
							IF tempM1 < x"5" THEN
								tempM1 <= STD_LOGIC_VECTOR(to_unsigned((to_integer(unsigned(tempM1)) + 1), 4));
							ELSE
								tempM1 <= x"0";
							END IF;
							
						WHEN 3 =>
							IF tempM0 < x"9" THEN
								tempM0 <= STD_LOGIC_VECTOR(to_unsigned((to_integer(unsigned(tempM0)) + 1), 4));
							ELSE
								tempM0 <= x"0";
							END IF;
							
						WHEN 4 =>
							IF tempS1 < x"5" THEN
								tempS1 <= STD_LOGIC_VECTOR(to_unsigned((to_integer(unsigned(tempS1)) + 1), 4));
							ELSE
								tempS1 <= x"0";
							END IF;
							
						WHEN 5 =>
							IF tempS0 < x"9" THEN
								tempS0 <= STD_LOGIC_VECTOR(to_unsigned((to_integer(unsigned(tempS0)) + 1), 4));
							ELSE
								tempS0 <= x"0";
							END IF;
					END CASE;	
				END IF;
			END IF;
			IF digit_n > 5 THEN
				digit_n <= 0;
			END IF;
			
	 END PROCESS;
    ----------------------
    -- Conversion time ---
    ----------------------
	 
    -- H_out1 binary value
	 H_out1_bin <= x"F"   WHEN (clk_1s = '0' AND (time_input = '1' OR alarm_input = '1') AND digit_n = 0)
				 ELSE tempH1 WHEN (time_input = '1' OR alarm_input = '1')
				 ELSE toBin1(counter_hour);
    -- 7-Segment LED display of H_out1
    convert_hex_H_out1 : bin2hex PORT MAP(Bin => H_out1_bin, Hout => H_out1);

    -- H_out0 binary value
	 H_out0_bin <= x"F"   WHEN (clk_1s = '0' AND (time_input = '1' OR alarm_input = '1') AND digit_n = 1)
				 ELSE tempH0 WHEN (time_input = '1' OR alarm_input = '1')
				 ELSE toBin0(counter_hour, H_out1_bin);
    -- 7-Segment LED display of H_out0
    convert_hex_H_out0 : bin2hex PORT MAP(Bin => H_out0_bin, Hout => H_out0);
	 
    -- M_out1 binary value
    M_out1_bin <= x"F"   WHEN (clk_1s = '0' AND (time_input = '1' OR alarm_input = '1') AND digit_n = 2)
				 ELSE tempM1 WHEN (time_input = '1' OR alarm_input = '1')
				 ELSE toBin1(counter_minute);
    -- 7-Segment LED display of M_out1
    convert_hex_M_out1 : bin2hex PORT MAP(Bin => M_out1_bin, Hout => M_out1);
	 
    -- M_out0 binary value
    M_out0_bin <= x"F"   WHEN (clk_1s = '0' AND (time_input = '1' OR alarm_input = '1') AND digit_n = 3)
				 ELSE tempM0 WHEN (time_input = '1' OR alarm_input = '1')
				 ELSE toBin0(counter_minute, M_out1_bin);
	 -- 7-Segment LED display of M_out0
    convert_hex_M_out0 : bin2hex PORT MAP(Bin => M_out0_bin, Hout => M_out0);
    
	 -- S_out1 binary value
    S_out1_bin <= x"F"   WHEN (clk_1s = '0' AND (time_input = '1' OR alarm_input = '1') AND digit_n = 4)
				 ELSE tempS1 WHEN (time_input = '1' OR alarm_input = '1')
				 ELSE toBin1(counter_second);
    -- 7-Segment LED display of S_out1
    convert_hex_S_out1 : bin2hex PORT MAP(Bin => S_out1_bin, Hout => S_out1);
	 
    -- S_out0 binary value
    S_out0_bin <= x"F"   WHEN (clk_1s = '0' AND (time_input = '1' OR alarm_input = '1') AND digit_n = 5)
				 ELSE tempS0 WHEN (time_input = '1' OR alarm_input = '1')
				 ELSE toBin0(counter_second, S_out1_bin);
    -- 7-Segment LED display of S_out0
    convert_hex_S_out0 : bin2hex PORT MAP(Bin => S_out0_bin, Hout => S_out0);
END Behavioral;

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY int2bin IS
    PORT (
        clk, tin, ain, i : IN STD_LOGIC;
        myDig, currDig : IN INTEGER;
        counter, temp : IN INTEGER;
        bin_out : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
    );
END int2bin;
ARCHITECTURE arch OF int2bin IS
    SIGNAL res : STD_LOGIC_VECTOR(3 DOWNTO 0);
BEGIN
    PROCESS(clk)
    BEGIN
        IF (tin = '1' OR ain = '1') THEN
            CASE(i) IS
                WHEN '1' => res <= STD_LOGIC_VECTOR(to_unsigned((temp / 10), 4));
                WHEN OTHERS => res <= STD_LOGIC_VECTOR(to_unsigned((temp MOD 10), 4));
            END CASE;
            IF myDig = currDig AND clk = '0' THEN -- blink if we are at current digit 
                res <= x"F"; -- blank
            END IF;
        ELSE
            CASE(i) IS
                WHEN '1' => res <= STD_LOGIC_VECTOR(to_unsigned((counter / 10), 4));
                WHEN OTHERS => res <= STD_LOGIC_VECTOR(to_unsigned((counter MOD 10), 4));
            END CASE;
        END IF;
    END PROCESS;
END arch;
-- BCD to HEX For 7-segment LEDs display 
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
ENTITY bin2hex IS
    PORT (
        Bin : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        Hout : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
    );
END bin2hex;
ARCHITECTURE Behavioral OF bin2hex IS
BEGIN
    PROCESS (Bin)
    BEGIN
        CASE(Bin) IS
            WHEN "0000" => Hout <= "1000000"; --0--
            WHEN "0001" => Hout <= "1111001"; --1--
            WHEN "0010" => Hout <= "0100100"; --2--
            WHEN "0011" => Hout <= "0110000"; --3--
            WHEN "0100" => Hout <= "0011001"; --4-- 
            WHEN "0101" => Hout <= "0010010"; --5--    
            WHEN "0110" => Hout <= "0000010"; --6--
            WHEN "0111" => Hout <= "1111000"; --7--   
            WHEN "1000" => Hout <= "0000000"; --8--
            WHEN "1001" => Hout <= "0010000"; --9--
            WHEN "1010" => Hout <= "0001000"; --a--
            WHEN "1011" => Hout <= "0000011"; --b--
            WHEN "1100" => Hout <= "1000110"; --c--
            WHEN "1101" => Hout <= "0100001"; --d--
            WHEN "1110" => Hout <= "0000110"; --e--
            WHEN OTHERS => Hout <= "1111111"; --BLANK--
        END CASE;
    END PROCESS;
END Behavioral;





--H_out1_bin <= x"F" WHEN (clk_1s = '0' AND time_input = '1' AND digit_n = 0)
--     ELSE
--     x"0" WHEN time_input = '1'
--     ELSE
--     x"2" WHEN counter_hour >= 20 ELSE
--     x"1" WHEN counter_hour >= 10 ELSE
--     x"0";