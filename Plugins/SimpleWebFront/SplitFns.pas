//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the SimpleWebFront-DBDesigner4-Plugin.
// Copyright (C) 2003 Bayer Ulrich
//
// The SimpleWebFront-DBDesigner4-Plugin is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// SimpleWebFront-DBDesigner4-Plugin is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------

//Most functions here were written by Henry Bartlett.
//I found them in a newsletter from a delphi-tips-page.
{#########################################}
{      Author: Henry Bartlett             }
{#########################################}

unit SplitFns;
interface
uses Classes;

function GetNextToken
  (Const S: string;
  Separator: char;
  var StartPos: integer): String;

{Returns the next token (substring)
from string S, starting at index
StartPos and ending 1 character 
before the next occurrence of
Separator (or at the end of S, 
whichever comes first).}
{StartPos returns the starting 
position for the next token, 1 
more than the position in S of 
the end of this token}

procedure Split
  (const S: String;
  Separator: Char;
  MyStringList: TStringList);

{Splits a string containing designated
separators into tokens and adds
them to MyStringList NOTE: MyStringList
must be Created before being passed to this
procedure and Freed after use}

function AddToken 
  (const aToken, S: String;
  Separator: Char;
  StringLimit: integer): String;

{Used to join 2 strings with a 
separator character between them and
can be used in a Join function}
{The StringLimit parameter prevents 
the length of the Result String
from exceeding a preset maximum}


//this one is by Ulrich Bayer
procedure IntelligentSplit(s:String; delimiter:Char; result: TStringList);

const delimiter = ';';

implementation
Uses Sysutils, StrUtils;







//splits the string at all delimiter-positions if the delimiter
//is not inside a () -pair
procedure IntelligentSplit(s:String; delimiter:Char; result: TStringList);
var i, bracket,beginPos :Integer;
begin
  beginPos:= 1;
  i:= 1;
  bracket:= 0;
  //what shoule we do when the string s ends already with a delimiter?
  if (s <> '') THEN s:= s + delimiter; //so we can add the last part too

  while (i <= Length(s)) do
  begin

    if (s[i] = '(') then bracket:= bracket+1
    else if (s[i] = ')') then bracket:= bracket-1
    else if ((s[i] = delimiter) and (bracket < 1)) then
    begin
      result.Add(midBStr(s,beginPos,i-beginPos));
      beginPos := i+1;
    end;
    i := i+1;
  end;

end;







function GetNextToken
  (Const S: string;
  Separator: char;
  var StartPos: integer): String;
var Index: integer;
begin
  Result := '';

{Step over repeated separators}
  While (S[StartPos] = Separator)
  and (StartPos <= length(S))do
   StartPos := StartPos + 1;

  if StartPos > length(S) then Exit;

{Set Index to StartPos}
  Index := StartPos;

{Find the next Separator}
  While (S[Index] <> Separator)
  and (Index <= length(S))do
   Index := Index + 1;

{Copy the token to the Result}
  Result := Copy(S, StartPos, Index - StartPos);

{SetStartPos to next Character after the Separator}
  StartPos := Index + 1;
end;

procedure Split
  (const S: String;
  Separator: Char;
  MyStringList: TStringList);
var Start: integer;
begin
  Start := 1;
  While Start <= Length(S) do
    MyStringList.Add
      (GetNextToken(S, Separator, Start));
end;

function AddToken (const aToken, S: String;
                   Separator: Char;
                   StringLimit: integer): String;
begin
  if Length(aToken) + Length(S) < StringLimit then
    begin
      {Add a separator unless the 
       Result string is empty}
      if S = '' then
        Result := ''
      else Result := S + Separator;

      {Add the token}
      Result := Result + aToken;
    end
  else
  {if the StringLimit would be 
  exceeded, raise an exception}
    Raise Exception.Create('Cannot add token');
end;

end.
