-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): xbujzad00 <xbujzad00@stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
  -- FSM
  type states is (
    S_RESET,
    S_INIT,
    S_FETCH,
    S_DECODE,
    S_INC_VAL, S_INC_START,
    S_DEC_VAL, S_DEC_START,
    S_INC_PTR, S_DEC_PTR,
    S_PRINT, S_STALL_PRINT,
    S_INPUT, S_STALL_INPUT, S_READ_INPUT,
    S_TEMP_START_R, S_TEMP_R,
    S_TEMP_START_W, S_TEMP_W,
    S_WHILE, S_WHILE_2, S_WHILE_3, S_WHILE_4,
    S_WHILE_END, S_WHILE_END_2, S_WHILE_END_3, S_WHILE_END_4,
    S_HALT,
    S_SKIP
  );

  signal currState : states := S_RESET;
  signal nextState : states := S_RESET;

  -- TEMP Register
  signal temp : std_logic_vector(7 downto 0);
  signal tempId : std_logic;

  -- CNT register
  signal cnt : std_logic_vector(7 downto 0);
  signal cntInc : std_logic;
  signal cntDec : std_logic;

  -- PC register
  signal pc : std_logic_vector(12 downto 0);
  signal pcInc : std_logic;
  signal pcDec : std_logic;

  -- PTR register
  signal ptr : std_logic_vector(12 downto 0);
  signal ptrInc : std_logic;
  signal ptrDec : std_logic;

  -- Memory address multiplexor
  signal muxAddress : std_logic;

  -- Data write multiplexor
  signal muxWrite : std_logic_vector(1 downto 0);

begin

  switchState: process(CLK, RESET)
  begin
    if RESET = '1' then
      currState <= S_RESET;
    elsif rising_edge(CLK) then
      if EN = '1' then
        currState <= nextState;
      end if;
    end if;
  end process;

  stateLogic: process(currState, DATA_RDATA)
  begin
    OUT_DATA <= DATA_RDATA;
    DATA_RDWR <= '0';
    tempId <= '1';
    muxWrite <= "00";
    muxAddress <= '0';
    OUT_WE <= '0';
    OUT_INV <= '0';
    DATA_EN <= '0';
    IN_REQ <= '0';
    pcInc <= '0';
    pcDec <= '0';
    ptrInc <= '0';
    ptrDec <= '0';
    cntInc <= '0';
    cntDec <= '0';

    case currState is 
      when S_RESET =>
        READY <= '0';
        DONE <= '0';
        nextState <= S_INIT;
      when S_INIT =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        muxAddress <= '0';
        if DATA_RDATA = x"40" then
          READY <= '1';
          nextState <= S_FETCH;
        else 
          ptrInc <= '1';
          nextState <= S_INIT;
        end if;
      when S_FETCH =>
        muxAddress <= '1';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        nextState <= S_DECODE;
      when S_DECODE =>
        case DATA_RDATA is
          when x"40" => -- @
            nextState <= S_HALT;
          when x"2B" => -- +
            nextState <= S_INC_START;
          when x"2D" => -- -
            nextState <= S_DEC_START;
          when x"3E" => -- >
            nextState <= S_INC_PTR;
          when x"3C" => -- <
            nextState <= S_DEC_PTR;
          when x"2E" => -- .
            nextState <= S_PRINT;
          when x"2C" => -- ,
            nextState <= S_INPUT;
          when X"24" => -- $
            nextState <= S_TEMP_START_R;
          when X"21" => -- !
            nextState <= S_TEMP_START_W;
          when X"5B" =>
            nextState <= S_WHILE;
          when X"5D" =>
            nextState <= S_WHILE_END;
          when others =>
            nextState <= S_SKIP;
        end case;
      when S_INC_START =>
        muxAddress <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        nextState <= S_INC_VAL;
      when S_INC_VAL =>
        muxWrite <= "11";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        muxAddress <= '0';
        pcInc <= '1';
        nextState <= S_FETCH;
      when S_DEC_START =>
        muxAddress <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        nextState <= S_DEC_VAL;
      when S_DEC_VAL =>
        muxWrite <= "10";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        muxAddress <= '0';
        pcInc <= '1';
        nextState <= S_FETCH;
      when S_INC_PTR =>
        ptrInc <= '1';
        pcInc <= '1';
        nextState <= S_FETCH;
      when S_DEC_PTR =>
        ptrDec <= '1';
        pcInc <= '1';
        nextState <= S_FETCH;
      when S_PRINT =>
        muxAddress <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        nextState <= S_STALL_PRINT;
      when S_STALL_PRINT =>
        if OUT_BUSY = '0' then
          OUT_DATA <= DATA_RDATA;
          OUT_wE <= '1';
          pcInc <= '1';
          nextState <= S_FETCH;
        else
          nextState <= S_PRINT;
        end if;
      when S_INPUT =>
        IN_REQ <= '1';
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        muxAddress <= '0';
        nextState <= S_STALL_INPUT;
      when S_STALL_INPUT =>
        if IN_VLD = '1' then
          DATA_EN <= '1';
          DATA_RDWR <= '0';
          IN_REQ <= '1';
          muxWrite <= "00";
          nextState <= S_READ_INPUT;
        else 
          IN_REQ <= '1';
          nextState <= S_INPUT;
        end if;
      when S_READ_INPUT =>
        DATA_RDWR <= '1';
        pcInc <= '1';
        nextState <= S_FETCH;
        when S_TEMP_START_R =>
        muxAddress <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        nextState <= S_TEMP_R;
      when S_TEMP_R =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        tempId <= '0';
        muxAddress <= '0';
        pcInc <= '1';
        nextState <= S_FETCH;
      when S_TEMP_START_W =>
        muxAddress <= '0';
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        nextState <= S_TEMP_W;
      when S_TEMP_W =>
        muxWrite <= "01";
        DATA_EN <= '1';
        DATA_RDWR <= '0';
        pcInc <= '1';
        nextState <= S_FETCH;
      when S_WHILE =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        muxAddress <= '0';
        pcInc <= '1';
        nextState <= S_WHILE_2;
      when S_WHILE_2 =>
        if DATA_RDATA = x"00" then
          cntInc <= '1';
          nextState <= S_WHILE_3;
        else
          nextState <= S_FETCH;
        end if ;
      when S_WHILE_3 =>
        if cnt = "00000000" then
          nextState <= S_FETCH;
        else
          DATA_EN <= '1';
          DATA_RDWR <= '1';
          muxAddress <= '1';
          nextState <= S_WHILE_4;
        end if;
      when S_WHILE_4 =>
        if DATA_RDATA = x"5D" then
          cntDec <= '1';
        elsif DATA_RDATA = x"5B" then
          cntInc <= '1';
        end if;
        pcInc <= '1';
        nextState <= S_WHILE_3;
      when S_WHILE_END =>
        DATA_EN <= '1';
        DATA_RDWR <= '1';
        muxAddress <= '0';
        nextState <= S_WHILE_END_2;
      when S_WHILE_END_2 =>
        if DATA_RDATA = x"00" then
          pcInc <= '1';
          nextState <= S_FETCH;
        else
          cntInc <= '1';
          pcDec <= '1';
          nextState <= S_WHILE_END_3;
        end if;
      when S_WHILE_END_3 =>
        if cnt = "00000000" then
          pcInc <= '1';
          nextState <= S_FETCH;
        else
          DATA_EN <= '1';
          DATA_RDWR <= '1';
          muxAddress <= '1';
          nextState <= S_WHILE_END_4;
        end if;
      when S_WHILE_END_4 =>
        if DATA_RDATA = x"5B" then
          cntDec <= '1';
        elsif DATA_RDATA = x"5D" then
          cntInc <= '1';
        end if;
        pcDec <= '1';
        nextState <= S_WHILE_END_3;
      when S_SKIP =>
        pcInc <= '1';
        nextState <= S_FETCH;
      when S_HALT =>
        DONE <= '1';
        nextState <= S_HALT;
      when others =>
        null;
    end case;
  end process;

  -- PC register
  p_PC: process(CLK, RESET)
  begin 
    if RESET = '1' then
      pc <= (others => '0');
    elsif rising_edge(CLK) then
      if pcInc = '1' then
        pc <= pc + 1;
      elsif pcDec = '1' then
        pc <= pc - 1;
      end if;
    end if;
  end process;

  -- PTR register
  p_PTR: process(CLK, RESET)
  begin
    if RESET = '1' then
      ptr <= (others => '0');
    elsif rising_edge(CLK) then
      if ptrInc = '1' then
        ptr <= ptr + 1;
      elsif ptrDec = '1' then
        ptr <= ptr - 1;
      end if;
    end if;
  end process;

  -- CNT register
  p_CNT: process(CLK, RESET)
  begin
    if RESET = '1' then
      cnt <= (others => '0');
    elsif rising_edge(CLK) then
      if cntInc = '1' then
        cnt <= cnt + 1;
      elsif cntDec = '1' then
        cnt <= cnt - 1;
      end if;
    end if;
  end process;
  
  -- TEMP register
  p_TEMP : process(CLK, RESET, DATA_RDATA, temp, tempId)
  begin
    if RESET = '1' then
      temp <= x"00";
    elsif rising_edge(CLK) then
      if tempId = '0' then
        temp <= DATA_RDATA;
      end if;
    end if;
  end process;

  -- MX1 register
  p_MX1: process(muxAddress, pc, ptr)
  begin
    if muxAddress = '0' then
      DATA_ADDR <= ptr;
    elsif muxAddress = '1' then
      DATA_ADDR <= pc;
    else
      DATA_ADDR <= (others => '0');
    end if;
  end process;

  -- MX2 register
  p_MX2 : process (muxWrite, IN_DATA, DATA_RDATA)
  begin
    case muxWrite is
      when "00" =>
        DATA_WDATA <= IN_DATA;
      when "01" =>
        DATA_WDATA <= temp;
      when "10" =>
        DATA_WDATA <= DATA_RDATA - 1;
      when "11" =>
        DATA_WDATA <= DATA_RDATA + 1;
      when others => null;
    end case;
  end process;

end behavioral; 
