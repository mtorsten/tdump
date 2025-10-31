program test_tdump;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  { Enumeration for different output modes }
  TOutputMode = (omByte, omWord, omDWord);
  
  { Configuration record to store all program options }
  TDumpConfig = record
    Count: Integer;           // Number of bytes per line
    OutputMode: TOutputMode;  // Byte, word, or dword grouping
    Verbose: Boolean;         // Enable verbose output
    InputFile: string;        // Input filename
    UseStdIn: Boolean;        // Read from standard input
    ShowHelp: Boolean;        // Display help information
  end;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

{ Test helper procedures }
procedure AssertEqual(Expected, Actual: Integer; const TestName: string);
begin
  if Expected = Actual then
  begin
    WriteLn('PASS: ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: ', TestName, ' - Expected: ', Expected, ', Actual: ', Actual);
    Inc(TestsFailed);
  end;
end;

procedure AssertEqual(Expected, Actual: string; const TestName: string);
begin
  if Expected = Actual then
  begin
    WriteLn('PASS: ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: ', TestName);
    WriteLn('  Expected: "', Expected, '"');
    WriteLn('  Actual:   "', Actual, '"');
    Inc(TestsFailed);
  end;
end;

procedure AssertEqual(Expected, Actual: Boolean; const TestName: string);
begin
  if Expected = Actual then
  begin
    WriteLn('PASS: ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: ', TestName, ' - Expected: ', Expected, ', Actual: ', Actual);
    Inc(TestsFailed);
  end;
end;

procedure AssertEqual(Expected, Actual: TOutputMode; const TestName: string);
const
  ModeNames: array[TOutputMode] of string = ('omByte', 'omWord', 'omDWord');
begin
  if Expected = Actual then
  begin
    WriteLn('PASS: ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: ', TestName, ' - Expected: ', ModeNames[Expected], ', Actual: ', ModeNames[Actual]);
    Inc(TestsFailed);
  end;
end;

{ Core functions from tdump.pas - copied for testing }

{ Validate and adjust count value based on output mode }
function ValidateAndAdjustCount(Count: Integer; Mode: TOutputMode): Integer;
begin
  Result := Count;
  
  case Mode of
    omByte:
      begin
        // Byte mode: minimum count is 1
        if Result < 1 then
          Result := 1;
      end;
    omWord:
      begin
        // Word mode: minimum count is 2, must be even
        if Result < 2 then
          Result := 2
        else if (Result mod 2) <> 0 then
          Result := (Result div 2) * 2; // Round down to nearest even number
      end;
    omDWord:
      begin
        // DWord mode: minimum count is 4, must be multiple of 4
        if Result < 4 then
          Result := 4
        else if (Result mod 4) <> 0 then
          Result := (Result div 4) * 4; // Round down to nearest multiple of 4
      end;
  end;
end;

{ Initialize configuration with default values }
procedure InitializeConfig(var AConfig: TDumpConfig);
begin
  AConfig.Count := 16;              // Default 16 bytes per line
  AConfig.OutputMode := omByte;     // Default byte mode
  AConfig.Verbose := False;         // No verbose output by default
  AConfig.InputFile := '';          // No input file by default
  AConfig.UseStdIn := True;         // Use stdin by default
  AConfig.ShowHelp := False;        // Don't show help by default
end;

{ Calculate number of output lines based on file size and bytes per line }
function CalculateLineCount(FileSize: Int64; BytesPerLine: Integer): Integer;
begin
  if FileSize <= 0 then
    Result := 0
  else
    Result := (FileSize + BytesPerLine - 1) div BytesPerLine; // Ceiling division
end;

{ Format verbose header/footer line with file information }
function FormatVerboseLine(const FileName: string; FileSize: Int64; LineCount: Integer; IsHeader: Boolean): string;
var
  DateTime: TDateTime;
  DateTimeStr: string;
  SizeStr: string;
  LineCountStr: string;
  FileNameStr: string;
  Prefix: string;
begin
  // Get current date and time in ISO format
  DateTime := Now;
  DateTimeStr := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', DateTime);
  
  // Format file size
  if FileSize >= 0 then
    SizeStr := IntToStr(FileSize) + ' bytes'
  else
    SizeStr := 'unknown size';
  
  // Format line count
  LineCountStr := IntToStr(LineCount) + ' lines';
  
  // Handle filename display - different for file vs stdin
  if FileName = '' then
    FileNameStr := 'stdin'
  else
    FileNameStr := FileName;
  
  // Choose prefix based on header/footer
  if IsHeader then
    Prefix := 'Begin'
  else
    Prefix := 'End';
  
  // Format the complete verbose line
  Result := Format('%s dump of %s at %s (%s, %s)', 
    [Prefix, FileNameStr, DateTimeStr, SizeStr, LineCountStr]);
end;

{ Format a single line of hexadecimal output }
function FormatHexLine(const Buffer: array of Byte; Count: Integer; Offset: Int64; Mode: TOutputMode; ExpectedBytesPerLine: Integer): string;
var
  i: Integer;
  HexPart, AsciiPart: string;
  ByteValue: Byte;
  AsciiChar: Char;
  GroupSize: Integer;
  ExpectedHexWidth: Integer;
begin
  // Initialize parts
  HexPart := '';
  AsciiPart := '';
  
  // Format offset as 8-digit hexadecimal
  Result := Format('%.8X  ', [Offset]);
  
  // Determine group size based on mode
  case Mode of
    omByte: GroupSize := 1;   // 1 byte per group (2 hex digits)
    omWord: GroupSize := 2;   // 2 bytes per group (4 hex digits)
    omDWord: GroupSize := 4;  // 4 bytes per group (8 hex digits)
  end;
  
  // Process each byte in the buffer
  for i := 0 to Count - 1 do
  begin
    ByteValue := Buffer[i];
    
    // Add hex representation (always 2 digits per byte)
    HexPart := HexPart + Format('%.2X', [ByteValue]);
    
    // Add space after complete groups (but not after the last byte)
    if (i < Count - 1) and ((i + 1) mod GroupSize = 0) then
      HexPart := HexPart + ' ';
    
    // Add ASCII representation
    AsciiChar := Chr(ByteValue);
    if (ByteValue >= 32) and (ByteValue <= 126) then
      AsciiPart := AsciiPart + AsciiChar  // Printable character (32-126)
    else
      AsciiPart := AsciiPart + '.';       // Non-printable character
  end;
  
  // Pad hex part to consistent width for alignment based on expected bytes per line
  // This ensures the ASCII part is always aligned, even for partial last lines
  case Mode of
    omByte:
      // Byte mode: 2 chars per byte + 1 space between each byte (except last)
      // For 16 bytes: "01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10" = 47 chars
      if ExpectedBytesPerLine > 1 then
        ExpectedHexWidth := ExpectedBytesPerLine * 3 - 1
      else
        ExpectedHexWidth := ExpectedBytesPerLine * 2;
    omWord:
      // Word mode: 4 chars per word + 1 space between words (except last)
      // For 16 bytes (8 words): "0102 0304 0506 0708 090A 0B0C 0D0E 0F10" = 39 chars
      if ExpectedBytesPerLine > 2 then
        ExpectedHexWidth := ExpectedBytesPerLine * 2 + (ExpectedBytesPerLine div 2) - 1
      else
        ExpectedHexWidth := ExpectedBytesPerLine * 2;
    omDWord:
      // DWord mode: 8 chars per dword + 1 space between dwords (except last)
      // For 16 bytes (4 dwords): "01020304 05060708 090A0B0C 0D0E0F10" = 35 chars
      if ExpectedBytesPerLine > 4 then
        ExpectedHexWidth := ExpectedBytesPerLine * 2 + (ExpectedBytesPerLine div 4) - 1
      else
        ExpectedHexWidth := ExpectedBytesPerLine * 2;
  end;
  
  // Pad to the expected width
  while Length(HexPart) < ExpectedHexWidth do
    HexPart := HexPart + ' ';
  
  // Combine all parts: offset + hex data + ASCII
  Result := Result + HexPart + '  ' + AsciiPart;
end;

{ Simplified command line parser for testing }
function ParseCommandLineSimple(const Args: array of string; var AConfig: TDumpConfig): Boolean;
var
  i: Integer;
  Param: string;
  CountValue: Integer;
  Code: Integer;
begin
  Result := True;
  i := 0;
  
  while i < Length(Args) do
  begin
    Param := Args[i];
    
    // Handle short and long form options
    if (Param = '-c') or (Param = '--count') then
    begin
      // Count option requires a parameter
      if i >= Length(Args) - 1 then
      begin
        Result := False;
        Exit;
      end;
      
      Inc(i);
      Val(Args[i], CountValue, Code);
      if Code <> 0 then
      begin
        Result := False;
        Exit;
      end;
      
      if CountValue < 1 then
      begin
        Result := False;
        Exit;
      end;
      
      AConfig.Count := CountValue;
    end
    else if (Param = '-w') or (Param = '--word') then
    begin
      AConfig.OutputMode := omWord;
      // Re-validate count for new mode
      AConfig.Count := ValidateAndAdjustCount(AConfig.Count, AConfig.OutputMode);
    end
    else if (Param = '-d') or (Param = '--dword') then
    begin
      AConfig.OutputMode := omDWord;
      // Re-validate count for new mode
      AConfig.Count := ValidateAndAdjustCount(AConfig.Count, AConfig.OutputMode);
    end
    else if (Param = '-v') or (Param = '--verbose') then
    begin
      AConfig.Verbose := True;
    end
    else if (Param = '-h') or (Param = '--help') then
    begin
      AConfig.ShowHelp := True;
    end
    else if Param[1] = '-' then
    begin
      // Unknown option
      Result := False;
      Exit;
    end
    else
    begin
      // This is the filename (last non-option argument)
      AConfig.InputFile := Param;
      AConfig.UseStdIn := False;
    end;
    
    Inc(i);
  end;
  
  // Validate and adjust count based on output mode
  AConfig.Count := ValidateAndAdjustCount(AConfig.Count, AConfig.OutputMode);
end;

{ Test procedures }

procedure TestValidateAndAdjustCount;
begin
  WriteLn('=== Testing ValidateAndAdjustCount ===');
  
  // Test byte mode
  AssertEqual(1, ValidateAndAdjustCount(0, omByte), 'Byte mode: count 0 should become 1');
  AssertEqual(1, ValidateAndAdjustCount(1, omByte), 'Byte mode: count 1 should remain 1');
  AssertEqual(16, ValidateAndAdjustCount(16, omByte), 'Byte mode: count 16 should remain 16');
  
  // Test word mode
  AssertEqual(2, ValidateAndAdjustCount(0, omWord), 'Word mode: count 0 should become 2');
  AssertEqual(2, ValidateAndAdjustCount(1, omWord), 'Word mode: count 1 should become 2');
  AssertEqual(2, ValidateAndAdjustCount(2, omWord), 'Word mode: count 2 should remain 2');
  AssertEqual(2, ValidateAndAdjustCount(3, omWord), 'Word mode: count 3 should become 2');
  AssertEqual(16, ValidateAndAdjustCount(16, omWord), 'Word mode: count 16 should remain 16');
  AssertEqual(16, ValidateAndAdjustCount(17, omWord), 'Word mode: count 17 should become 16');
  
  // Test dword mode
  AssertEqual(4, ValidateAndAdjustCount(0, omDWord), 'DWord mode: count 0 should become 4');
  AssertEqual(4, ValidateAndAdjustCount(1, omDWord), 'DWord mode: count 1 should become 4');
  AssertEqual(4, ValidateAndAdjustCount(3, omDWord), 'DWord mode: count 3 should become 4');
  AssertEqual(4, ValidateAndAdjustCount(4, omDWord), 'DWord mode: count 4 should remain 4');
  AssertEqual(4, ValidateAndAdjustCount(5, omDWord), 'DWord mode: count 5 should become 4');
  AssertEqual(16, ValidateAndAdjustCount(16, omDWord), 'DWord mode: count 16 should remain 16');
  AssertEqual(16, ValidateAndAdjustCount(17, omDWord), 'DWord mode: count 17 should become 16');
end;

procedure TestInitializeConfig;
var
  Config: TDumpConfig;
begin
  WriteLn('=== Testing InitializeConfig ===');
  
  InitializeConfig(Config);
  
  AssertEqual(16, Config.Count, 'Default count should be 16');
  AssertEqual(omByte, Config.OutputMode, 'Default output mode should be omByte');
  AssertEqual(False, Config.Verbose, 'Default verbose should be False');
  AssertEqual('', Config.InputFile, 'Default input file should be empty');
  AssertEqual(True, Config.UseStdIn, 'Default UseStdIn should be True');
  AssertEqual(False, Config.ShowHelp, 'Default ShowHelp should be False');
end;

procedure TestCalculateLineCount;
begin
  WriteLn('=== Testing CalculateLineCount ===');
  
  AssertEqual(0, CalculateLineCount(0, 16), 'Empty file should have 0 lines');
  AssertEqual(0, CalculateLineCount(-1, 16), 'Negative size should have 0 lines');
  AssertEqual(1, CalculateLineCount(1, 16), '1 byte should have 1 line');
  AssertEqual(1, CalculateLineCount(16, 16), '16 bytes should have 1 line');
  AssertEqual(2, CalculateLineCount(17, 16), '17 bytes should have 2 lines');
  AssertEqual(2, CalculateLineCount(32, 16), '32 bytes should have 2 lines');
  AssertEqual(3, CalculateLineCount(33, 16), '33 bytes should have 3 lines');
  
  // Test with different bytes per line
  AssertEqual(2, CalculateLineCount(16, 8), '16 bytes with 8 per line should have 2 lines');
  AssertEqual(3, CalculateLineCount(17, 8), '17 bytes with 8 per line should have 3 lines');
end;

procedure TestFormatHexLine;
var
  Buffer: array[0..15] of Byte;
  i: Integer;
  Result: string;
begin
  WriteLn('=== Testing FormatHexLine ===');
  
  // Initialize test buffer with known values
  for i := 0 to 15 do
    Buffer[i] := i + 1; // 01, 02, 03, ..., 10
  
  // Test byte mode
  Result := FormatHexLine(Buffer, 4, 0, omByte, 16);
  AssertEqual('00000000  01 02 03 04                                      ....', Result, 'Byte mode: 4 bytes at offset 0');
  
  Result := FormatHexLine(Buffer, 16, $100, omByte, 16);
  AssertEqual('00000100  01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10  ................', Result, 'Byte mode: 16 bytes at offset 256');
  
  // Test word mode
  Result := FormatHexLine(Buffer, 8, 0, omWord, 16);
  AssertEqual('00000000  0102 0304 0506 0708                      ........', Result, 'Word mode: 8 bytes at offset 0');
  
  // Test dword mode
  Result := FormatHexLine(Buffer, 8, 0, omDWord, 16);
  AssertEqual('00000000  01020304 05060708                    ........', Result, 'DWord mode: 8 bytes at offset 0');
end;

procedure TestFormatHexLineWithPrintableChars;
var
  Buffer: array[0..15] of Byte;
  i: Integer;
  Result: string;
begin
  WriteLn('=== Testing FormatHexLine with Printable Characters ===');
  
  // Initialize buffer with "Hello World!" (printable) + some non-printable
  Buffer[0] := Ord('H');   // 72
  Buffer[1] := Ord('e');   // 101
  Buffer[2] := Ord('l');   // 108
  Buffer[3] := Ord('l');   // 108
  Buffer[4] := Ord('o');   // 111
  Buffer[5] := Ord(' ');   // 32 (space)
  Buffer[6] := Ord('W');   // 87
  Buffer[7] := Ord('o');   // 111
  Buffer[8] := Ord('r');   // 114
  Buffer[9] := Ord('l');   // 108
  Buffer[10] := Ord('d');  // 100
  Buffer[11] := Ord('!');  // 33
  Buffer[12] := 10;        // LF (non-printable)
  Buffer[13] := 13;        // CR (non-printable)
  Buffer[14] := 0;         // NULL (non-printable)
  Buffer[15] := 255;       // 0xFF (non-printable)
  
  Result := FormatHexLine(Buffer, 16, 0, omByte, 16);
  AssertEqual('00000000  48 65 6C 6C 6F 20 57 6F 72 6C 64 21 0A 0D 00 FF  Hello World!....', Result, 'Mixed printable and non-printable characters');
end;

procedure TestCommandLineParsing;
var
  Config: TDumpConfig;
  Args: array of string;
begin
  WriteLn('=== Testing Command Line Parsing ===');
  
  // Test default configuration
  InitializeConfig(Config);
  SetLength(Args, 0);
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'Empty args should parse successfully');
  AssertEqual(16, Config.Count, 'Default count should remain 16');
  AssertEqual(omByte, Config.OutputMode, 'Default mode should remain omByte');
  
  // Test count option
  InitializeConfig(Config);
  SetLength(Args, 2);
  Args[0] := '-c';
  Args[1] := '8';
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'Count option should parse successfully');
  AssertEqual(8, Config.Count, 'Count should be set to 8');
  
  // Test word mode
  InitializeConfig(Config);
  SetLength(Args, 1);
  Args[0] := '-w';
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'Word option should parse successfully');
  AssertEqual(omWord, Config.OutputMode, 'Mode should be set to omWord');
  AssertEqual(16, Config.Count, 'Count should remain 16 (valid for word mode)');
  
  // Test dword mode
  InitializeConfig(Config);
  SetLength(Args, 1);
  Args[0] := '-d';
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'DWord option should parse successfully');
  AssertEqual(omDWord, Config.OutputMode, 'Mode should be set to omDWord');
  AssertEqual(16, Config.Count, 'Count should remain 16 (valid for dword mode)');
  
  // Test verbose option
  InitializeConfig(Config);
  SetLength(Args, 1);
  Args[0] := '-v';
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'Verbose option should parse successfully');
  AssertEqual(True, Config.Verbose, 'Verbose should be set to True');
  
  // Test help option
  InitializeConfig(Config);
  SetLength(Args, 1);
  Args[0] := '-h';
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'Help option should parse successfully');
  AssertEqual(True, Config.ShowHelp, 'ShowHelp should be set to True');
  
  // Test filename
  InitializeConfig(Config);
  SetLength(Args, 1);
  Args[0] := 'test.bin';
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'Filename should parse successfully');
  AssertEqual('test.bin', Config.InputFile, 'InputFile should be set to test.bin');
  AssertEqual(False, Config.UseStdIn, 'UseStdIn should be set to False');
  
  // Test combined options
  InitializeConfig(Config);
  SetLength(Args, 4);
  Args[0] := '-c';
  Args[1] := '8';
  Args[2] := '-w';
  Args[3] := 'test.bin';
  AssertEqual(True, ParseCommandLineSimple(Args, Config), 'Combined options should parse successfully');
  AssertEqual(8, Config.Count, 'Count should be set to 8');
  AssertEqual(omWord, Config.OutputMode, 'Mode should be set to omWord');
  AssertEqual('test.bin', Config.InputFile, 'InputFile should be set to test.bin');
  AssertEqual(False, Config.UseStdIn, 'UseStdIn should be set to False');
end;

procedure TestFormatVerboseLine;
var
  Result: string;
begin
  WriteLn('=== Testing FormatVerboseLine ===');
  
  // Test header for file
  Result := FormatVerboseLine('test.bin', 1024, 64, True);
  // We can't test the exact datetime, but we can check the structure
  if (Pos('Begin dump of test.bin at', Result) = 1) and 
     (Pos('(1024 bytes, 64 lines)', Result) > 0) then
  begin
    WriteLn('PASS: FormatVerboseLine header structure');
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: FormatVerboseLine header structure');
    WriteLn('  Result: "', Result, '"');
    Inc(TestsFailed);
  end;
  
  // Test footer for file
  Result := FormatVerboseLine('test.bin', 1024, 64, False);
  if (Pos('End dump of test.bin at', Result) = 1) and 
     (Pos('(1024 bytes, 64 lines)', Result) > 0) then
  begin
    WriteLn('PASS: FormatVerboseLine footer structure');
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: FormatVerboseLine footer structure');
    WriteLn('  Result: "', Result, '"');
    Inc(TestsFailed);
  end;
  
  // Test stdin
  Result := FormatVerboseLine('', -1, 10, False);
  if (Pos('End dump of stdin at', Result) = 1) and 
     (Pos('(unknown size, 10 lines)', Result) > 0) then
  begin
    WriteLn('PASS: FormatVerboseLine stdin structure');
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: FormatVerboseLine stdin structure');
    WriteLn('  Result: "', Result, '"');
    Inc(TestsFailed);
  end;
end;

{ Main test execution }
begin
  WriteLn('Running tdump unit tests...');
  WriteLn;
  
  TestsPassed := 0;
  TestsFailed := 0;
  
  TestValidateAndAdjustCount;
  WriteLn;
  
  TestInitializeConfig;
  WriteLn;
  
  TestCalculateLineCount;
  WriteLn;
  
  TestFormatHexLine;
  WriteLn;
  
  TestFormatHexLineWithPrintableChars;
  WriteLn;
  
  TestCommandLineParsing;
  WriteLn;
  
  TestFormatVerboseLine;
  WriteLn;
  
  WriteLn('=== Test Results ===');
  WriteLn('Tests passed: ', TestsPassed);
  WriteLn('Tests failed: ', TestsFailed);
  WriteLn('Total tests: ', TestsPassed + TestsFailed);
  
  if TestsFailed = 0 then
  begin
    WriteLn('All tests passed!');
    Halt(0);
  end
  else
  begin
    WriteLn('Some tests failed!');
    Halt(1);
  end;
end.