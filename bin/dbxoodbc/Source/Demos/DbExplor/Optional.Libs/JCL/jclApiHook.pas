unit jclApiHook;

// http://wasm.ru/article.php?article=apihook_2
// http://wasm.ru/pub/21/files/advapihook.rar

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils;

function SizeOfCode(Code: pointer): dword;
function SizeOfProc(Proc: pointer): dword;
function SaveOldFunction(Proc: pointer; Old: pointer): dword;
function HookCode(TargetProc, NewProc: pointer; var OldProc: pointer): boolean;
function UnhookCode(OldProc: pointer): boolean;
function HookProc(lpModuleName, lpProcName: PChar; NewProc: pointer; var OldProc: pointer): boolean;

implementation

const
  Opcodes1: array [0..255] of word =
  (
    $4211, $42E4, $2011, $20E4, $8401, $8C42, $0000, $0000, $4211, $42E4,
    $2011, $20E4, $8401, $8C42, $0000, $0000, $4211, $42E4, $2011, $20E4,

    $8401, $8C42, $0000, $0000, $4211, $42E4, $2011, $20E4, $8401, $8C42,

    $0000, $0000, $4211, $42E4, $2011, $20E4, $8401, $8C42, $0000, $8000,

    $4211, $42E4, $2011, $20E4, $8401, $8C42, $0000, $8000, $4211, $42E4,

    $2011, $20E4, $8401, $8C42, $0000, $8000, $0211, $02E4, $0011, $00E4,

    $0401, $0C42, $0000, $8000, $6045, $6045, $6045, $6045, $6045, $6045,

    $6045, $6045, $6045, $6045, $6045, $6045, $6045, $6045, $6045, $6045,

    $0045, $0045, $0045, $0045, $0045, $0045, $0045, $0045, $6045, $6045,

    $6045, $6045, $6045, $6045, $6045, $6045, $0000, $8000, $00E4, $421A,

    $0000, $0000, $0000, $0000, $0C00, $2CE4, $0400, $24E4, $0000, $0000,

    $0000, $0000, $1400, $1400, $1400, $1400, $1400, $1400, $1400, $1400,

    $1400, $1400, $1400, $1400, $1400, $1400, $1400, $1400, $0510, $0DA0,

    $0510, $05A0, $0211, $02E4, $A211, $A2E4, $4211, $42E4, $2011, $20E4,

    $42E3, $20E4, $00E3, $01A0, $0000, $E046, $E046, $E046, $E046, $E046,

    $E046, $E046, $8000, $0000, $0000, $0000, $0000, $0000, $0000, $8000,

    $8101, $8142, $0301, $0342, $0000, $0000, $0000, $0000, $0401, $0C42,

    $0000, $0000, $8000, $8000, $0000, $0000, $6404, $6404, $6404, $6404,

    $6404, $6404, $6404, $6404, $6C45, $6C45, $6C45, $6C45, $6C45, $6C45,

    $6C45, $6C45, $4510, $45A0, $0800, $0000, $20E4, $20E4, $4510, $4DA0,

    $0000, $0000, $0800, $0000, $0000, $0400, $0000, $0000, $4110, $41A0,

    $4110, $41A0, $8400, $8400, $0000, $8000, $0008, $0008, $0008, $0008,

    $0008, $0008, $0008, $0008, $1400, $1400, $1400, $1400, $8401, $8442,

    $0601, $0642, $1C00, $1C00, $0000, $1400, $8007, $8047, $0207, $0247,

    $0000, $0000, $0000, $0000, $0000, $0000, $0008, $0008, $0000, $0000,

    $0000, $0000, $0000, $0000, $4110, $01A0
  );

  Opcodes2: array [0..255] of word =
  (
    $0118, $0120, $20E4, $20E4, $FFFF, $0000, $0000, $0000, $0000, $0000,
    $FFFF, $FFFF, $FFFF, $0110, $0000, $052D, $003F, $023F, $003F, $023F,

    $003F, $003F, $003F, $023F, $0110, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF,

    $FFFF, $FFFF, $4023, $4023, $0223, $0223, $FFFF, $FFFF, $FFFF, $FFFF,

    $003F, $023F, $002F, $023F, $003D, $003D, $003F, $003F, $0000, $8000,

    $8000, $8000, $0000, $0000, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF,

    $FFFF, $FFFF, $FFFF, $FFFF, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4,

    $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4, $20E4,

    $4227, $003F, $003F, $003F, $003F, $003F, $003F, $003F, $003F, $003F,

    $003F, $003F, $003F, $003F, $003F, $003F, $00ED, $00ED, $00ED, $00ED,

    $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $0065, $00ED, $04ED, $04A8, $04A8, $04A8, $00ED, $00ED, $00ED, $0000,

    $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $0265, $02ED, $1C00, $1C00,

    $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00, $1C00,

    $1C00, $1C00, $1C00, $1C00, $4110, $4110, $4110, $4110, $4110, $4110,

    $4110, $4110, $4110, $4110, $4110, $4110, $4110, $4110, $4110, $4110,

    $0000, $0000, $8000, $02E4, $47E4, $43E4, $C211, $C2E4, $0000, $0000,

    $0000, $42E4, $47E4, $43E4, $0020, $20E4, $C211, $C2E4, $20E4, $42E4,

    $20E4, $22E4, $2154, $211C, $FFFF, $FFFF, $05A0, $42E4, $20E4, $20E4,

    $2154, $211C, $A211, $A2E4, $043F, $0224, $0465, $24AC, $043F, $8128,

    $6005, $6005, $6005, $6005, $6005, $6005, $6005, $6005, $FFFF, $00ED,

    $00ED, $00ED, $00ED, $00ED, $02ED, $20AC, $00ED, $00ED, $00ED, $00ED,

    $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $003F, $02ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $FFFF, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED, $00ED,

    $00ED, $00ED, $00ED, $00ED, $00ED, $0000
  );

  Opcodes3: array [0..9] of array [0..15] of word =
  (
     ($0510, $FFFF, $4110, $4110, $8110, $8110, $8110, $8110, $0510, $FFFF,
      $4110, $4110, $8110, $8110, $8110, $8110),
     ($0DA0, $FFFF, $41A0, $41A0, $81A0, $81A0, $81A0, $81A0, $0DA0, $FFFF,
      $41A0, $41A0, $81A0, $81A0, $81A0, $81A0),
     ($0120, $0120, $0120, $0120, $0120, $0120, $0120, $0120, $0036, $0036,
      $0030, $0030, $0036, $0036, $0036, $0036),
     ($0120, $FFFF, $0120, $0120, $0110, $0118, $0110, $0118, $0030, $0030,
      $0000, $0030, $0000, $0000, $0000, $0000),
     ($0120, $0120, $0120, $0120, $0120, $0120, $0120, $0120, $0036, $0036,
      $0036, $0036, $FFFF, $0000, $FFFF, $FFFF),
     ($0120, $FFFF, $0120, $0120, $FFFF, $0130, $FFFF, $0130, $0036, $0036,
      $0036, $0036, $0000, $0036, $0036, $0000),
     ($0128, $0128, $0128, $0128, $0128, $0128, $0128, $0128, $0236, $0236,
      $0030, $0030, $0236, $0236, $0236, $0236),
     ($0128, $FFFF, $0128, $0128, $0110, $FFFF, $0110, $0118, $0030, $0030,
      $0030, $0030, $0030, $0030, $FFFF, $FFFF),
     ($0118, $0118, $0118, $0118, $0118, $0118, $0118, $0118, $0236, $0236,
      $0030, $0236, $0236, $0236, $0236, $0236),
     ($0118, $FFFF, $0118, $0118, $0130, $0128, $0130, $0128, $0030, $0030,
      $0030, $0030, $0000, $0036, $0036, $FFFF)
  );

function SizeOfCode(Code: pointer): dword;
var
  Opcode: word;
  Modrm: byte;
  Fixed, AddressOveride: boolean;
  Last, OperandOveride, Flags, Rm, Size, Extend: dword;
begin
  try
    Last := dword(Code);
    if Code <> nil then
    begin
      AddressOveride := False;
      Fixed := False;
      OperandOveride := 4;
      Extend := 0;
      repeat
        Opcode := byte(Code^);
        Code := pointer(dword(Code) + 1);
        if Opcode = $66 then OperandOveride := 2
        else if Opcode = $67 then  AddressOveride := True
        else
        if not ((Opcode and $E7) = $26) then
         if not (Opcode in [$64..$65]) then  Fixed := True;
      until Fixed;
      if Opcode = $0f then
      begin
        Opcode := byte(Code^);
        Flags := Opcodes2[Opcode];
        Opcode := Opcode + $0f00;
        Code := pointer(dword(Code) + 1);
      end
      else Flags := Opcodes1[Opcode];

      if ((Flags and $0038) <> 0) then
      begin
        Modrm := byte(Code^);
        Rm := Modrm and $7;
        Code := pointer(dword(Code) + 1);

        case (Modrm and $c0) of
          $40: Size := 1;
          $80: if AddressOveride then Size := 2 else Size := 4;
          else Size := 0;
        end;

        if not (((Modrm and $c0) <> $c0) and AddressOveride) then
        begin
          if (Rm = 4) and ((Modrm and $c0) <> $c0) then Rm := byte(Code^) and $7;
          if ((Modrm and $c0 = 0) and (Rm = 5)) then Size := 4;
          Code := pointer(dword(Code) + Size);
        end;

        if ((Flags and $0038) = $0008) then
        begin
          case Opcode of
            $f6: Extend := 0;
            $f7: Extend := 1;
            $d8: Extend := 2;
            $d9: Extend := 3;
            $da: Extend := 4;
            $db: Extend := 5;
            $dc: Extend := 6;
            $dd: Extend := 7;
            $de: Extend := 8;
            $df: Extend := 9;
          end;
          if ((Modrm and $c0) <> $c0) then
            Flags := Opcodes3[Extend][(Modrm shr 3) and $7] else
            Flags := Opcodes3[Extend][((Modrm shr 3) and $7) + 8];
        end;

      end;
      case (Flags and $0C00) of
        $0400: Code := pointer(dword(Code) + 1);
        $0800: Code := pointer(dword(Code) + 2);
        $0C00: Code := pointer(dword(Code) + OperandOveride);
        else
        begin
          case Opcode of
            $9a, $ea: Code := pointer(dword(Code) + OperandOveride + 2);
            $c8: Code := pointer(dword(Code) + 3);
            $a0..$a3:
              begin
                if AddressOveride then
                  Code := pointer(dword(Code) + 2)
                  else Code := pointer(dword(Code) + 4);
              end;
          end;
        end;
      end;
    end;
    Result := dword(Code) - Last;
  except
    Result := 0;
  end;
end;

function SizeOfProc(Proc: pointer): dword;
var
  Length: dword;
begin
  Result := 0;
  repeat
    Length := SizeOfCode(Proc);
    Inc(Result, Length);
    if ((Length = 1) and (byte(Proc^) = $C3)) then Break;
    Proc := pointer(dword(Proc) + Length);
  until Length = 0;
end;

function SaveOldFunction(Proc: pointer; Old: pointer): dword;
var
  SaveSize, Size: dword;
  Next: pointer;
begin
  SaveSize := 0;
  Next := Proc;
  while SaveSize < 5 do
  begin
    Size := SizeOfCode(Next);
    Next := pointer(dword(Next) + Size);
    Inc(SaveSize, Size);
  end;
  CopyMemory(Old, Proc, SaveSize);
  byte(pointer(dword(Old) + SaveSize)^) := $e9;
  dword(pointer(dword(Old) + SaveSize + 1)^) := dword(Next) - dword(Old) - SaveSize - 5;
  Result := SaveSize;
end;

function HookCode(TargetProc, NewProc: pointer; var OldProc: pointer): boolean;
var
  Address: dword;
  OldProtect: dword;
  OldFunction: pointer;
  Proc: pointer;
begin
  Result := False;
  try
    Proc := TargetProc;
    Address := dword(NewProc) - dword(Proc) - 5;
    VirtualProtect(Proc, 255, PAGE_EXECUTE_READWRITE, OldProtect);
    GetMem(OldFunction, 255);
    dword(OldFunction^) := dword(Proc);
    byte(pointer(dword(OldFunction) + 4)^) := SaveOldFunction(Proc, pointer(dword(OldFunction) + 5));
    byte(Proc^) := $e9;
    dword(pointer(dword(Proc) + 1)^) := Address;
    VirtualProtect(Proc, 255, OldProtect, OldProtect);
    OldProc := pointer(dword(OldFunction) + 5);
  except
    Exit;
  end;
  Result := True;
end;

function HookProc(lpModuleName, lpProcName: PChar; NewProc: pointer; var OldProc: pointer): boolean;
var
 hModule: dword;
 fnAdr: pointer;
begin
 Result := false;
 hModule := GetModuleHandle(lpModuleName);
 if hModule = 0 then hModule := LoadLibrary(lpModuleName);
 if hModule = 0 then Exit;
 fnAdr := GetProcAddress(hModule, lpProcName);
 if fnAdr = nil then Exit;
 Result := HookCode(fnAdr, NewProc, OldProc);
end;

function UnhookCode(OldProc: pointer): boolean;
var
  OldProtect: dword;
  Proc: pointer;
  SaveSize: dword;
begin
  Result := True;
  try
    Proc := pointer(dword(pointer(dword(OldProc) - 5)^));
    SaveSize := byte(pointer(dword(OldProc) - 1)^);
    VirtualProtect(Proc, 5, PAGE_EXECUTE_READWRITE, OldProtect);
    CopyMemory(Proc, OldProc, SaveSize);
    VirtualProtect(Proc, 5, OldProtect, OldProtect);
    FreeMem(pointer(dword(OldProc) - 5));
  except
    Result := False;
  end;
end;

end.
