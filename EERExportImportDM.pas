unit EERExportImportDM;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE DBDesigner4.
// Copyright (C) 2002 Michael G. Zinner, www.fabFORCE.net
//
// DBDesigner4 is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// DBDesigner4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit EERExportImportDM.pas
// --------------------------
// Version 2.1, 18.09.2003, Mike
// Description
//   Contains Import and Export procedures
//
// Changes:
//   Version 1.0, 18.09.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

{$I DBDesigner4.inc}

interface

uses
  SysUtils, Classes, QTypes, IniFiles, QDialogs, EERModel;

type
  TDMEERExportImport = class(TDataModule)
    procedure ImportERwin41XMLModel(theModel: TEERModel; fname: string);
    procedure ExportMDBXMLFile(theModel: TEERModel; fname: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMEERExportImport: TDMEERExportImport;

implementation

{$R *.xfm}

uses  MainDM,
{$IFDEF USE_IXMLDBMODELType}
  EERModel_XML_ERwin41_Import,
{$ENDIF}
  EERDM, GUIDM;

procedure TDMEERExportImport.ImportERwin41XMLModel(theModel: TEERModel; fname: string);
{$IFDEF USE_IXMLDBMODELType}
var theDoc: IXMLERwin4Type;
  s, tbl_id, DatatypeName, DatatypeParams: WideString;
  i, j, k, l, x, y: Integer;
  theDataType: TEERDatatype;
  theTbl: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
  theRel: TEERRel;
  theSubstList: TStringList;
  theIniFile: TMemIniFile;
  NewRelCounter: Integer;
{$ENDIF}
begin
{$IFDEF USE_IXMLDBMODELType}
  if(fname='')then
    Exit;

  NewRelCounter:=0;

  theIniFile:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_DatabaseInfo.ini');
  try
    theSubstList:=TStringList.Create;
    try
      theDoc:=LoadERwin4(fname);
      try
        //Get Model's name
        s:=theDoc.Model.ModelProps.Name;
        s:=Copy(s, 1, Length(s)-Length(ExtractFileExt(s)));
        theModel.SetModelName(s);

        //Get Default Datatype
        s:=theDoc.Model.ModelProps.Default_Datatype;
        theDataType:=theModel.GetDataTypeByName(s);
        if(theDataType=nil)then
        begin
          theDataType:=TEERDatatype.Create(theModel);
          theDataType.id:=DMMain.GetNextGlobalID;
          theDataType.TypeName:=s;
          theModel.Datatypes.Add(theDataType);
          theModel.CommonDataType.Add(IntToStr(theDataType.id));
        end;
        theModel.DefaultDataType:=theDataType.id;

        //Get Position Grid
        if(theDoc.Model.ModelProps.Layout_Grid<>0)then
        begin
          theModel.UsePositionGrid:=True;
          theModel.PositionGrid.X:=theDoc.Model.ModelProps.Layout_Grid_X;
          theModel.PositionGrid.Y:=theDoc.Model.ModelProps.Layout_Grid_Y;
        end;

        //Get Datatype Subst
        if(theDoc.Model.TargetServer=174)then
          s:='Oracle';

        s:=s+'_'+theModel.DatabaseType+'_DatatypeSubst';
        theIniFile.ReadSectionValues(s, theSubstList);

        // --------------------------------------------------------------
        // Metadata

        // Tables
        //Read Tables
        try
          for i:=0 to theDoc.Model.Entity_Groups.Count-1 do
          begin
            tbl_id:=theDoc.Model.Entity_Groups[i].id;

            //Get Positions from Subject_Area
            for j:=0 to theDoc.Model.Subject_Area_Groups.Subject_Area[0].Stored_Display_Groups.Stored_Display.Drawing_Object_Entity_Groups.Count-1 do
              if(theDoc.Model.Subject_Area_Groups.Subject_Area[0].Stored_Display_Groups.Stored_Display.Drawing_Object_Entity_Groups[j].Drawing_Object_EntityProps.DO_Reference_Object=tbl_id)then
              begin
                s:=theDoc.Model.Subject_Area_Groups.Subject_Area[0].Stored_Display_Groups.Stored_Display.Drawing_Object_Entity_Groups[j].Drawing_Object_EntityProps.DO_Location;
                break;
              end;

            try
              y:=Round(StrToInt(Copy(s, 2, Pos(',', s)-1-1))*1.3);
              s:=Copy(s, Pos(',', s)+1, Length(s));
              x:=Round(StrToInt(Copy(s, 1, Pos(',', s)-1))*1.3);
            except
              x:=0;
              y:=0;
            end;

            theTbl:=theModel.NewTable(x, y, False);
            theTbl.ObjName:=theDoc.Model.Entity_Groups[i].EntityProps.Physical_Name;
            if(Trim(theTbl.ObjName)='')then
              theTbl.ObjName:=theDoc.Model.Entity_Groups[i].Name;
            theTbl.tmp:=theDoc.Model.Entity_Groups[i].Id;

            theTbl.Comments:=theDoc.Model.Entity_Groups[i].EntityProps.Comment;

            //Get Columns
            for j:=0 to theDoc.Model.Entity_Groups[i].Attribute_Groups.Count-1 do
            begin
              theColumn:=TEERColumn.Create(theTbl);
              theColumn.Obj_id:=DMMain.GetNextGlobalID;
              theColumn.ColName:=theDoc.Model.Entity_Groups[i].Attribute_Groups.Attribute[j].AttributeProps.Physical_Name;
              if(theColumn.ColName='')then
                theColumn.ColName:=theDoc.Model.Entity_Groups[i].Attribute_Groups.Attribute[j].Name;

              //Store the ERwin-id in tmp
              theColumn.tmp:=theDoc.Model.Entity_Groups[i].Attribute_Groups.Attribute[j].Id;

              theColumn.Comments:=theDoc.Model.Entity_Groups[i].Attribute_Groups.Attribute[j].AttributeProps.Comment;

              //Get Datatype and params
              DatatypeName:=theDoc.Model.Entity_Groups[i].Attribute_Groups.Attribute[j].AttributeProps.Datatype;
              //If there is no DatatypeName, check Domain_Groups
              if(DatatypeName='')then
              begin
                for k:=0 to theDoc.Model.Domain_Groups.Count-1 do
                  if(theDoc.Model.Domain_Groups.Domain[k].Id=
                    theDoc.Model.Entity_Groups[i].Attribute_Groups.Attribute[j].AttributeProps.Parent_Domain)then
                  begin
                    DatatypeName:=theDoc.Model.Domain_Groups.Domain[k].DomainProps.Datatype;
                    break;
                  end;
              end;

              DatatypeParams:='';
              if(Pos('(', DatatypeName)>0)then
              begin
                DatatypeParams:=Copy(DatatypeName,
                  Pos('(', DatatypeName),
                  Pos(')', DatatypeName)-Pos('(', DatatypeName)+1);

                DatatypeName:=Copy(DatatypeName, 1, Pos('(', DatatypeName)-1);
              end;

              theColumn.idDatatype:=TEERDatatype(theModel.GetDataTypeByNameSubst(DatatypeName, theSubstList)).id;
              theColumn.DatatypeParams:=DatatypeParams;

              //NN
              try
                theColumn.NotNull:=(theDoc.Model.Entity_Groups[i].Attribute_Groups.Attribute[j].AttributeProps.Null_Option=1);
              except
                theColumn.NotNull:=False;
              end;

              //PK
              for k:=0 to theDoc.Model.Entity_Groups[i].Key_Group_Groups.Count-1 do
                if(theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[k].Key_GroupProps.Key_Group_Type='PK')then
                begin
                  for l:=0 to theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[k].Key_Group_Member_Groups.Count-1 do
                    if(theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[k].Key_Group_Member_Groups.Key_Group_Member[l].Key_Group_MemberProps.Key_Group_Member_Column=
                      theColumn.tmp)then
                    begin
                      theColumn.PrimaryKey:=True;
                      break;
                    end;

                  break;
                end;
              theTbl.Columns.Add(theColumn);
            end;

            //Get Indices
            for j:=0 to theDoc.Model.Entity_Groups[i].Key_Group_Groups.Count-1 do
            begin
              //ignore the PK
              if(theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[j].Key_GroupProps.Key_Group_Type='PK')then
                continue;

              theIndex:=TEERIndex.Create(self);

              theIndex.Obj_id:=DMMain.GetNextGlobalID;
              theIndex.IndexName:=theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[j].Name;
              if(Copy(theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[j].Key_GroupProps.Key_Group_Type, 1, 2)='AK')then
                theIndex.IndexKind:=ik_UNIQUE_INDEX
              else
                theIndex.IndexKind:=ik_INDEX;

              theIndex.Pos:=theTbl.Indices.Count;
              theTbl.Indices.Add(theIndex);

              for k:=0 to theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[j].Key_Group_Member_Groups.Count-1 do
              begin
                for l:=0 to theTbl.Columns.Count-1 do
                  if(theDoc.Model.Entity_Groups[i].Key_Group_Groups.Key_Group[j].Key_Group_Member_Groups.Key_Group_Member[k].Key_Group_MemberProps.Key_Group_Member_Column=
                    TEERColumn(theTbl.Columns[l]).tmp)then
                  begin
                    theIndex.Columns.Add(IntToStr(TEERColumn(theTbl.Columns[l]).Obj_id));
                    break;
                  end;
              end;
            end;

            theTbl.RefreshStrechedImg:=True;
          end;

          //Get Relations
          for i:=0 to theDoc.Model.Relationship_Groups.Count-1 do
          begin
            inc(NewRelCounter);
            theRel:=TEERRel.Create(theModel, 'Rel_'+FormatFloat('00', NewRelCounter));
            theRel.Obj_id:=DMMain.GetNextGlobalID;
            theRel.ObjName:=theDoc.Model.Relationship_Groups.Relationship[i].RelationshipProps.Name;
            if(Trim(theRel.ObjName)='')then
              theRel.ObjName:=theDoc.Model.Relationship_Groups.Relationship[i].Name;
            if(Trim(theRel.ObjName)='')then
              theRel.ObjName:=theRel.Name;

            theRel.Comments:=theDoc.Model.Relationship_Groups.Relationship[i].RelationshipProps.Note;
                          
            if(theDoc.Model.Relationship_Groups.Relationship[i].RelationshipProps.Type_=2)then
              theRel.RelKind:=rk_1n
            else
              theRel.RelKind:=rk_1nNonId;

            //SrcTable
            for j:=0 to theModel.ComponentCount-1 do
              if(theModel.Components[j].ClassNameIs('TEERTable'))then
                if(TEERTable(theModel.Components[j]).tmp=
                  theDoc.Model.Relationship_Groups.Relationship[i].RelationshipProps.Relationship_Parent_Entity)then
                begin
                  theRel.SrcTbl:=TEERTable(theModel.Components[j]);
                  break;
                end;

            //DestTable
            for j:=0 to theModel.ComponentCount-1 do
              if(theModel.Components[j].ClassNameIs('TEERTable'))then
                if(TEERTable(theModel.Components[j]).tmp=
                  theDoc.Model.Relationship_Groups.Relationship[i].RelationshipProps.Relationship_Child_Entity)then
                begin
                  theRel.DestTbl:=TEERTable(theModel.Components[j]);
                  break;
                end;

            //Add relation to Tables
            theRel.SrcTbl.RelStart.Add(theRel);
            theRel.DestTbl.RelEnd.Add(theRel);

            //Display at the right pos and size
            theRel.SrcTbl.RefreshRelations;
            theRel.DestTbl.RefreshRelations;
          end;
        except
          on x: Exception do
          begin
            ShowMessage('An Error occurred while reading Tables from XML File:'+#13#10#13#10+
              'Error: '+x.Message);
          end;
        end;
      finally
      end;

      theModel.ModelHasChanged;

      theModel.Refresh;
      DMEER.RefreshPalettes;
    finally
      theSubstList.Free;
    end;
  finally
    theIniFile.Free;
  end;
{$ENDIF}
end;

procedure TDMEERExportImport.ExportMDBXMLFile(theModel: TEERModel; fname: string);
var theFile: Textfile;
  theDBInfo: TMemIniFile;
  theDatatypeMappings, theInserts: TStringList;
  i, j, k: integer;
  theObjList: TList;
  physicalDatatypeName, s: string;
begin
  AssignFile(theFile, fname);
  Rewrite(theFile);
  try
    theDatatypeMappings:=TStringList.Create;
    theInserts:=TStringList.Create;
    try
      theDBInfo:=TMemIniFile.Create(DMMain.SettingsPath+'DBDesignerFork_DatabaseInfo.ini');
      try
        theDBInfo.ReadSectionValues(theModel.DatabaseType+'_MDB_DatatypeSubst', theDatatypeMappings);
      finally
        theDBInfo.Free;
      end;

      WriteLn(theFile, '<?xml version="1.0" encoding="ISO-8859-1" ?>');
      WriteLn(theFile, '<!--'+ExtractFileName(fname)+#13#10+
        'Generated '+FormatDateTime('yyyy-mm-dd hh:nn', Now)+#13#10+
        '-->');

      WriteLn(theFile, '<database>');
      WriteLn(theFile, '<name>'+theModel.GetModelName+'</name>');
      WriteLn(theFile, '<create>1</create>');

      theObjList:=TList.Create;
      try
        theModel.GetEERObjectList([EERTable], theObjList);
        theModel.SortEERObjectListByObjName(theObjList);

        for i:=0 to theObjList.Count-1 do
        begin
          WriteLn(theFile, '<table>');
          WriteLn(theFile, '<name>'+TEERTable(theObjList[i]).ObjName+'</name>');

          WriteLn(theFile, '<declaration>');

          //Columns
          for j:=0 to TEERTable(theObjList[i]).Columns.Count-1 do
          begin
            WriteLn(theFile, '<field>');
            WriteLn(theFile, '<name>'+TEERColumn(TEERTable(theObjList[i]).Columns[j]).ColName+'</name>');
            //Datatype mapped
            physicalDatatypeName:=TEERDatatype(theModel.GetDataType(TEERColumn(TEERTable(theObjList[i]).Columns[j]).iddatatype)).GetPhysicalTypeName;
            //if there is a ( in the physicalDatatypeName, truncate physicalDatatypeName
            if(Pos('(', physicalDatatypeName)>0)then
              physicalDatatypeName:=Copy(physicalDatatypeName, 1, Pos('(', physicalDatatypeName)-1);
            WriteLn(theFile, '<type>'+theDatatypeMappings.Values[physicalDatatypeName]+'</type>');
            //Default Value
            WriteLn(theFile, '<default>'+TEERColumn(TEERTable(theObjList[i]).Columns[j]).DefaultValue+'</default>');
            WriteLn(theFile, '<notnull>'+IntToStr(Ord(TEERColumn(TEERTable(theObjList[i]).Columns[j]).NotNull=True))+'</notnull>');
            WriteLn(theFile, '</field>');
          end;

          //Indices
          for j:=0 to TEERTable(theObjList[i]).Indices.Count-1 do
          begin
            WriteLn(theFile, '<index>');
            WriteLn(theFile, '<name>'+TEERIndex(TEERTable(theObjList[i]).Indices[j]).IndexName+'</name>');
            if(TEERIndex(TEERTable(theObjList[i]).Indices[j]).IndexKind=ik_PRIMARY)or
              (TEERIndex(TEERTable(theObjList[i]).Indices[j]).IndexKind=ik_UNIQUE_INDEX)then
              WriteLn(theFile, '<unique>1</unique>')
            else
              WriteLn(theFile, '<unique>0</unique>');

            WriteLn(theFile, '<field>');
            //Index fields
            for k:=0 to TEERIndex(TEERTable(theObjList[i]).Indices[j]).Columns.Count-1 do
              WriteLn(theFile, '<name>'+TEERColumn(TEERTable(theObjList[i]).GetColumnByID(StrToInt(TEERIndex(TEERTable(theObjList[i]).Indices[j]).Columns[k]))).ColName+'</name>');
            WriteLn(theFile, '</field>');

            WriteLn(theFile, '</index>');
          end;

          WriteLn(theFile, '</declaration>');

          WriteLn(theFile, '<initialization>');

          //put all lines from insert to one line for theInserts
          theInserts.Clear;
          s:='';
          for j:=0 to TEERTable(theObjList[i]).StandardInserts.Count-1 do
          begin
            s:=s+TEERTable(theObjList[i]).StandardInserts[j];

            if(Copy(Trim(s), Length(Trim(s)), 1)=';')then
            begin
              theInserts.Add(Copy(Trim(s), 1, Length(Trim(s))-1));
              s:='';
            end;
          end;

          for j:=0 to theInserts.Count-1 do
          begin
            //Invalid Select statement
            if(DMMain.GetSubStringCountInString(theInserts[j], '(')<2)then
              Continue;

            WriteLn(theFile, '<insert>');

            for k:=0 to TEERTable(theObjList[i]).Columns.Count-1 do
            begin
              s:=DMMain.GetValueFromSQLInsert(TEERColumn(TEERTable(theObjList[i]).Columns[k]).ColName, theInserts[j]);
              if(s<>'NOTININSERT')then
              begin
                WriteLn(theFile, '<field>');
                WriteLn(theFile, '<name>'+TEERColumn(TEERTable(theObjList[i]).Columns[k]).ColName+'</name>');
                WriteLn(theFile, '<value>'+s+'</value>');
                WriteLn(theFile, '</field>');
              end;
            end;

            WriteLn(theFile, '</insert>');
          end;


          WriteLn(theFile, '</initialization>');

          WriteLn(theFile, '</table>');
        end;

      finally
        theObjList.Free;
      end;
      WriteLn(theFile, '</database>');
    finally
      theDatatypeMappings.Free;
      theInserts.Free;
    end;
  finally
    CloseFile(theFile)
  end;

  DMGUI.SetStatusCaption(DMMain.GetTranslatedMessage('The model was successfully saved to %s.', 198, fname));
end;


end.
