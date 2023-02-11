---1HZ CLOCK
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity clk_div is
    port(
        CLK50MHZ: in std_logic;
--        CPU_RESETN: in std_logic;
--        reset: in std_logic;
        clk_stb: buffer std_logic
    );
end clk_div;

architecture b1 of clk_div is
    constant half_period: integer := 25000000; ---was 50M changed to 25M
    signal counter: integer range 0 to half_period - 1 := 0;
    signal ncount: std_logic := '0';

begin
    process(CLK50MHZ)
    begin
        if rising_edge(CLK50MHZ) then
--            if CPU_RESETN = '0' then
--                counter <= 0;
--                clk_stb <= '0';
--                ncount <= '0';
--            elsif reset = '1' and ncount = '0' then
--                counter <= 0;
--                clk_stb <= '0';
--                ncount <= '1';
--            els
				if counter = half_period - 1 then
                counter <= 0;
                clk_stb <= not clk_stb;
            else
                counter <= counter + 1;
            end if;
            
--            if reset ='0' then
--                ncount <= '0';
--            end if;
        end if;
    end process;
end b1;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
package clk_div_pkg is
component clk_div is
port(
        CLK50MHZ: in std_logic;
--        CPU_RESETN: in std_logic;
--        reset: in std_logic;
        clk_stb: buffer std_logic
    );
end component;
end clk_div_pkg;
