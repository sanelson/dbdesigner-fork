unit EERModel_XML;

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
// Unit EERModel_XML.pas
// ---------------------
// Version 1.3, 11.04.2003, Mike
// Description
//   Contains the XML Data Binding for the DBDesigner4 XML Model files
//
// Changes:
//   Version 1.3, 11.04.2003, Mike
//     added UsePositionGrid, DefaultTableType, TableNameInRefs, ActivateRefDefForNewRelations
//       Datatype-EditParamsAsString 
//   Version 1.2, 28.03.2003, Mike
//     added UsePosition Params
//   Version 1.1, 20.03.2003, Mike
//     added PrevTableName and index-column length parameter
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLDBMODELType = interface;
  IXMLSETTINGSType = interface;
  IXMLGLOBALSETTINGSType = interface;
  IXMLDATATYPEGROUPSType = interface;
  IXMLDATATYPEGROUPType = interface;
  IXMLDATATYPESType = interface;
  IXMLDATATYPEType = interface;
  IXMLPARAMSType = interface;
  IXMLPARAMType = interface;
  IXMLOPTIONSType = interface;
  IXMLOPTIONType = interface;
  IXMLCOMMON_DATATYPESType = interface;
  IXMLCOMMON_DATATYPEType = interface;
  IXMLTABLEPREFIXESType = interface;
  IXMLTABLEPREFIXType = interface;
  IXMLREGIONCOLORSType = interface;
  IXMLREGIONCOLORType = interface;
  IXMLPOSITIONMARKERSType = interface;
  IXMLPOSITIONMARKERType = interface;
  IXMLMETADATAType = interface;
  IXMLREGIONSType = interface;
  IXMLREGIONType = interface;
  IXMLTABLESType = interface;
  IXMLTABLEType = interface;
  IXMLCOLUMNSType = interface;
  IXMLCOLUMNType = interface;
  IXMLOPTIONSELECTEDType = interface;
  IXMLOPTIONSELECTType = interface;
  IXMLRELATIONS_STARTType = interface;
  IXMLRELATION_STARTType = interface;
  IXMLRELATIONS_ENDType = interface;
  IXMLRELATION_ENDType = interface;
  IXMLINDICESType = interface;
  IXMLINDEXType = interface;
  IXMLINDEXCOLUMNSType = interface;
  IXMLINDEXCOLUMNType = interface;
  IXMLRELATIONSType = interface;
  IXMLRELATIONType = interface;
  IXMLNOTESType = interface;
  IXMLNOTEType = interface;
  IXMLIMAGESType = interface;
  IXMLIMAGEType = interface;
  IXMLPLUGINDATAType = interface;
  IXMLPLUGINDATARECORDSType = interface;
  IXMLPLUGINDATARECORDType = interface;
  IXMLPLUGINDATAPARAMSType = interface;
  IXMLPLUGINDATAPARAMType = interface;
  IXMLQUERYDATAType = interface;
  IXMLQUERYRECORDSType = interface;
  IXMLQUERYRECORDType = interface;
  IXMLLINKEDMODELSType = interface;
  IXMLLINKEDMODELType = interface;

{ IXMLDBMODELType }

  IXMLDBMODELType = interface(IXMLNode)
    ['{CD469CD4-9ADB-4706-A30F-84B0A1E5F182}']
    { Property Accessors }
    function Get_Version: WideString;
    function Get_SETTINGS: IXMLSETTINGSType;
    function Get_METADATA: IXMLMETADATAType;
    function Get_PLUGINDATA: IXMLPLUGINDATAType;
    function Get_QUERYDATA: IXMLQUERYDATAType;
    function Get_LINKEDMODELS: IXMLLINKEDMODELSType;
    procedure Set_Version(Value: WideString);
    { Methods & Properties }
    property Version: WideString read Get_Version write Set_Version;
    property SETTINGS: IXMLSETTINGSType read Get_SETTINGS;
    property METADATA: IXMLMETADATAType read Get_METADATA;
    property PLUGINDATA: IXMLPLUGINDATAType read Get_PLUGINDATA;
    property QUERYDATA: IXMLQUERYDATAType read Get_QUERYDATA;
    property LINKEDMODELS: IXMLLINKEDMODELSType read Get_LINKEDMODELS;
  end;

{ IXMLSETTINGSType }

  IXMLSETTINGSType = interface(IXMLNode)
    ['{6B3D5680-9AA1-4F15-A70E-E19EC46E9B9B}']
    { Property Accessors }
    function Get_GLOBALSETTINGS: IXMLGLOBALSETTINGSType;
    function Get_DATATYPEGROUPS: IXMLDATATYPEGROUPSType;
    function Get_DATATYPES: IXMLDATATYPESType;
    function Get_COMMON_DATATYPES: IXMLCOMMON_DATATYPESType;
    function Get_TABLEPREFIXES: IXMLTABLEPREFIXESType;
    function Get_REGIONCOLORS: IXMLREGIONCOLORSType;
    function Get_POSITIONMARKERS: IXMLPOSITIONMARKERSType;
    { Methods & Properties }
    property GLOBALSETTINGS: IXMLGLOBALSETTINGSType read Get_GLOBALSETTINGS;
    property DATATYPEGROUPS: IXMLDATATYPEGROUPSType read Get_DATATYPEGROUPS;
    property DATATYPES: IXMLDATATYPESType read Get_DATATYPES;
    property COMMON_DATATYPES: IXMLCOMMON_DATATYPESType read Get_COMMON_DATATYPES;
    property TABLEPREFIXES: IXMLTABLEPREFIXESType read Get_TABLEPREFIXES;
    property REGIONCOLORS: IXMLREGIONCOLORSType read Get_REGIONCOLORS;
    property POSITIONMARKERS: IXMLPOSITIONMARKERSType read Get_POSITIONMARKERS;
  end;

{ IXMLGLOBALSETTINGSType }

  IXMLGLOBALSETTINGSType = interface(IXMLNode)
    ['{C3F13DE9-9ADF-408B-896F-090695160A9A}']
    { Property Accessors }
    function Get_ModelName: WideString;
    function Get_IDModel: Integer;
    function Get_IDVersion: Integer;
    function Get_VersionStr: WideString;
    function Get_Comments: WideString;
    function Get_UseVersionHistroy: Integer;
    function Get_AutoIncVersion: Integer;
    function Get_DatabaseType: WideString;
    function Get_ZoomFac: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_DefaultDataType: Integer;
    function Get_DefaultTablePrefix: Integer;
    function Get_DefSaveDBConn: WideString;
    function Get_DefSyncDBConn: WideString;
    function Get_DefQueryDBConn: WideString;
    function Get_Printer: WideString;
    function Get_HPageCount: WideString;
    function Get_PageAspectRatio: WideString;
    function Get_PageOrientation: Integer;
    function Get_PageFormat: WideString;
    function Get_SelectedPages: WideString;
    function Get_UsePositionGrid: Integer;
    function Get_PositionGridX: Integer;
    function Get_PositionGridY: Integer;
    function Get_TableNameInRefs: Integer;
    function Get_DefaultTableType: Integer;
    function Get_ActivateRefDefForNewRelations: Integer;
    function Get_FKPrefix: WideString;
    function Get_FKPostfix: WideString;
    function Get_CreateFKRefDefIndex: Integer;
    function Get_DBQuoteCharacter: WideString;
    function Get_CreateSQLforLinkedObjects: Integer;
    function Get_DefModelFont: WideString;
    function Get_CanvasWidth: Integer;
    function Get_CanvasHeight: Integer;
    procedure Set_ModelName(Value: WideString);
    procedure Set_IDModel(Value: Integer);
    procedure Set_IDVersion(Value: Integer);
    procedure Set_VersionStr(Value: WideString);
    procedure Set_Comments(Value: WideString);
    procedure Set_UseVersionHistroy(Value: Integer);
    procedure Set_AutoIncVersion(Value: Integer);
    procedure Set_DatabaseType(Value: WideString);
    procedure Set_ZoomFac(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_DefaultDataType(Value: Integer);
    procedure Set_DefaultTablePrefix(Value: Integer);
    procedure Set_DefSaveDBConn(Value: WideString);
    procedure Set_DefSyncDBConn(Value: WideString);
    procedure Set_DefQueryDBConn(Value: WideString);
    procedure Set_Printer(Value: WideString);
    procedure Set_HPageCount(Value: WideString);
    procedure Set_PageAspectRatio(Value: WideString);
    procedure Set_PageOrientation(Value: Integer);
    procedure Set_PageFormat(Value: WideString);
    procedure Set_SelectedPages(Value: WideString);
    procedure Set_UsePositionGrid(Value: Integer);
    procedure Set_PositionGridX(Value: Integer);
    procedure Set_PositionGridY(Value: Integer);
    procedure Set_TableNameInRefs(Value: Integer);
    procedure Set_DefaultTableType(Value: Integer);
    procedure Set_ActivateRefDefForNewRelations(Value: Integer);
    procedure Set_FKPrefix(Value: WideString);
    procedure Set_FKPostfix(Value: WideString);
    procedure Set_CreateFKRefDefIndex(Value: Integer);
    procedure Set_DBQuoteCharacter(Value: WideString);
    procedure Set_CreateSQLforLinkedObjects(Value: Integer);
    procedure Set_DefModelFont(Value: WideString);
    procedure Set_CanvasWidth(Value: Integer);
    procedure Set_CanvasHeight(Value: Integer);
    { Methods & Properties }
    property ModelName: WideString read Get_ModelName write Set_ModelName;
    property IDModel: Integer read Get_IDModel write Set_IDModel;
    property IDVersion: Integer read Get_IDVersion write Set_IDVersion;
    property VersionStr: WideString read Get_VersionStr write Set_VersionStr;
    property Comments: WideString read Get_Comments write Set_Comments;
    property UseVersionHistroy: Integer read Get_UseVersionHistroy write Set_UseVersionHistroy;
    property AutoIncVersion: Integer read Get_AutoIncVersion write Set_AutoIncVersion;
    property DatabaseType: WideString read Get_DatabaseType write Set_DatabaseType;
    property ZoomFac: WideString read Get_ZoomFac write Set_ZoomFac;
    property XPos: Integer read Get_XPos write Set_XPos;
    property YPos: Integer read Get_YPos write Set_YPos;
    property DefaultDataType: Integer read Get_DefaultDataType write Set_DefaultDataType;
    property DefaultTablePrefix: Integer read Get_DefaultTablePrefix write Set_DefaultTablePrefix;
    property DefSaveDBConn: WideString read Get_DefSaveDBConn write Set_DefSaveDBConn;
    property DefSyncDBConn: WideString read Get_DefSyncDBConn write Set_DefSyncDBConn;
    property DefQueryDBConn: WideString read Get_DefQueryDBConn write Set_DefQueryDBConn;
    property Printer: WideString read Get_Printer write Set_Printer;
    property HPageCount: WideString read Get_HPageCount write Set_HPageCount;
    property PageAspectRatio: WideString read Get_PageAspectRatio write Set_PageAspectRatio;
    property PageOrientation: Integer read Get_PageOrientation write Set_PageOrientation;
    property PageFormat: WideString read Get_PageFormat write Set_PageFormat;
    property SelectedPages: WideString read Get_SelectedPages write Set_SelectedPages;
    property UsePositionGrid: Integer read Get_UsePositionGrid write Set_UsePositionGrid;
    property PositionGridX: Integer read Get_PositionGridX write Set_PositionGridX;
    property PositionGridY: Integer read Get_PositionGridY write Set_PositionGridY;
    property TableNameInRefs: Integer read Get_TableNameInRefs write Set_TableNameInRefs;
    property DefaultTableType: Integer read Get_DefaultTableType write Set_DefaultTableType;
    property ActivateRefDefForNewRelations: Integer read Get_ActivateRefDefForNewRelations write Set_ActivateRefDefForNewRelations;
    property FKPrefix: WideString read Get_FKPrefix write Set_FKPrefix;
    property FKPostfix: WideString read Get_FKPostfix write Set_FKPostfix;
    property CreateFKRefDefIndex: Integer read Get_CreateFKRefDefIndex write Set_CreateFKRefDefIndex;
    property DBQuoteCharacter: WideString read Get_DBQuoteCharacter write Set_DBQuoteCharacter;
    property CreateSQLforLinkedObjects: Integer read Get_CreateSQLforLinkedObjects write Set_CreateSQLforLinkedObjects;
    property DefModelFont: WideString read Get_DefModelFont write Set_DefModelFont;
    property CanvasWidth: Integer read Get_CanvasWidth write Set_CanvasWidth;
    property CanvasHeight: Integer read Get_CanvasHeight write Set_CanvasHeight;
  end;

{ IXMLDATATYPEGROUPSType }

  IXMLDATATYPEGROUPSType = interface(IXMLNodeCollection)
    ['{BDFEA2B2-0243-411F-BF23-57FC8CE16153}']
    { Property Accessors }
    function Get_DATATYPEGROUP(Index: Integer): IXMLDATATYPEGROUPType;
    { Methods & Properties }
    function Add: IXMLDATATYPEGROUPType;
    function Insert(const Index: Integer): IXMLDATATYPEGROUPType;
    property DATATYPEGROUP[Index: Integer]: IXMLDATATYPEGROUPType read Get_DATATYPEGROUP; default;
  end;

{ IXMLDATATYPEGROUPType }

  IXMLDATATYPEGROUPType = interface(IXMLNode)
    ['{181A43B3-E773-46A8-966E-7976034B39DF}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Icon: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Icon(Value: Integer);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Icon: Integer read Get_Icon write Set_Icon;
  end;

{ IXMLDATATYPESType }

  IXMLDATATYPESType = interface(IXMLNodeCollection)
    ['{E1C85DE2-B775-4AB5-9FA5-CDD5CF73D52A}']
    { Property Accessors }
    function Get_DATATYPE(Index: Integer): IXMLDATATYPEType;
    { Methods & Properties }
    function Add: IXMLDATATYPEType;
    function Insert(const Index: Integer): IXMLDATATYPEType;
    property DATATYPE[Index: Integer]: IXMLDATATYPEType read Get_DATATYPE; default;
  end;

{ IXMLDATATYPEType }

  IXMLDATATYPEType = interface(IXMLNode)
    ['{FDFEE111-CEEB-4218-9BB2-80CB3D16117E}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_IDGroup: Integer;
    function Get_TypeName: WideString;
    function Get_Description: WideString;
    function Get_ParamCount: Integer;
    function Get_OptionCount: Integer;
    function Get_ParamRequired: Integer;
    function Get_EditParamsAsString: Integer;
    function Get_SynonymGroup: Integer;
    function Get_PhysicalMapping: Integer;
    function Get_PhysicalTypeName: WideString;
    function Get_PARAMS: IXMLPARAMSType;
    function Get_OPTIONS: IXMLOPTIONSType;
    procedure Set_ID(Value: Integer);
    procedure Set_IDGroup(Value: Integer);
    procedure Set_TypeName(Value: WideString);
    procedure Set_Description(Value: WideString);
    procedure Set_ParamCount(Value: Integer);
    procedure Set_OptionCount(Value: Integer);
    procedure Set_ParamRequired(Value: Integer);
    procedure Set_EditParamsAsString(Value: Integer);
    procedure Set_SynonymGroup(Value: Integer);
    procedure Set_PhysicalMapping(Value: Integer);
    procedure Set_PhysicalTypeName(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property IDGroup: Integer read Get_IDGroup write Set_IDGroup;
    property TypeName: WideString read Get_TypeName write Set_TypeName;
    property Description: WideString read Get_Description write Set_Description;
    property ParamCount: Integer read Get_ParamCount write Set_ParamCount;
    property OptionCount: Integer read Get_OptionCount write Set_OptionCount;
    property ParamRequired: Integer read Get_ParamRequired write Set_ParamRequired;
    property EditParamsAsString: Integer read Get_EditParamsAsString write Set_EditParamsAsString;
    property SynonymGroup: Integer read Get_SynonymGroup write Set_SynonymGroup;
    property PhysicalMapping: Integer read Get_PhysicalMapping write Set_PhysicalMapping;
    property PhysicalTypeName: WideString read Get_PhysicalTypeName write Set_PhysicalTypeName;
    property PARAMS: IXMLPARAMSType read Get_PARAMS;
    property OPTIONS: IXMLOPTIONSType read Get_OPTIONS;
  end;

{ IXMLPARAMSType }

  IXMLPARAMSType = interface(IXMLNodeCollection)
    ['{4352AF49-04D6-4437-A7A3-E8A0F4D64132}']
    { Property Accessors }
    function Get_PARAM(Index: Integer): IXMLPARAMType;
    { Methods & Properties }
    function Add: IXMLPARAMType;
    function Insert(const Index: Integer): IXMLPARAMType;
    property PARAM[Index: Integer]: IXMLPARAMType read Get_PARAM; default;
  end;

{ IXMLPARAMType }

  IXMLPARAMType = interface(IXMLNode)
    ['{7CF7E96F-7051-4F06-A357-23F9CBC82352}']
    { Property Accessors }
    function Get_Name: WideString;
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
  end;

{ IXMLOPTIONSType }

  IXMLOPTIONSType = interface(IXMLNodeCollection)
    ['{23756FE8-C2C5-4941-ABD0-DDDB93043E31}']
    { Property Accessors }
    function Get_OPTION(Index: Integer): IXMLOPTIONType;
    { Methods & Properties }
    function Add: IXMLOPTIONType;
    function Insert(const Index: Integer): IXMLOPTIONType;
    property OPTION[Index: Integer]: IXMLOPTIONType read Get_OPTION; default;
  end;

{ IXMLOPTIONType }

  IXMLOPTIONType = interface(IXMLNode)
    ['{871AF365-38C1-4121-86FC-102822695F32}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Default: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Default(Value: Integer);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Default: Integer read Get_Default write Set_Default;
  end;

{ IXMLCOMMON_DATATYPESType }

  IXMLCOMMON_DATATYPESType = interface(IXMLNodeCollection)
    ['{BA0ABA0A-752D-41EA-85AE-D1706EBC7C11}']
    { Property Accessors }
    function Get_COMMON_DATATYPE(Index: Integer): IXMLCOMMON_DATATYPEType;
    { Methods & Properties }
    function Add: IXMLCOMMON_DATATYPEType;
    function Insert(const Index: Integer): IXMLCOMMON_DATATYPEType;
    property COMMON_DATATYPE[Index: Integer]: IXMLCOMMON_DATATYPEType read Get_COMMON_DATATYPE; default;
  end;

{ IXMLCOMMON_DATATYPEType }

  IXMLCOMMON_DATATYPEType = interface(IXMLNode)
    ['{37F72A6A-CB3A-4911-A133-CCF4968F6F1B}']
    { Property Accessors }
    function Get_ID: Integer;
    procedure Set_ID(Value: Integer);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
  end;

{ IXMLTABLEPREFIXESType }

  IXMLTABLEPREFIXESType = interface(IXMLNodeCollection)
    ['{CCAEECCE-0AA3-44E4-B97C-133AE226BE31}']
    { Property Accessors }
    function Get_TABLEPREFIX(Index: Integer): IXMLTABLEPREFIXType;
    { Methods & Properties }
    function Add: IXMLTABLEPREFIXType;
    function Insert(const Index: Integer): IXMLTABLEPREFIXType;
    property TABLEPREFIX[Index: Integer]: IXMLTABLEPREFIXType read Get_TABLEPREFIX; default;
  end;

{ IXMLTABLEPREFIXType }

  IXMLTABLEPREFIXType = interface(IXMLNode)
    ['{30655844-1E34-438D-81D8-2F5A51332AF8}']
    { Property Accessors }
    function Get_Name: WideString;
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
  end;

{ IXMLREGIONCOLORSType }

  IXMLREGIONCOLORSType = interface(IXMLNodeCollection)
    ['{257F122A-5EC7-4107-BE33-5E7D1BBD9047}']
    { Property Accessors }
    function Get_REGIONCOLOR(Index: Integer): IXMLREGIONCOLORType;
    { Methods & Properties }
    function Add: IXMLREGIONCOLORType;
    function Insert(const Index: Integer): IXMLREGIONCOLORType;
    property REGIONCOLOR[Index: Integer]: IXMLREGIONCOLORType read Get_REGIONCOLOR; default;
  end;

{ IXMLREGIONCOLORType }

  IXMLREGIONCOLORType = interface(IXMLNode)
    ['{CCA39621-D47A-4A28-B418-F40E7DDA5FE0}']
    { Property Accessors }
    function Get_Color: WideString;
    procedure Set_Color(Value: WideString);
    { Methods & Properties }
    property Color: WideString read Get_Color write Set_Color;
  end;

{ IXMLPOSITIONMARKERSType }

  IXMLPOSITIONMARKERSType = interface(IXMLNodeCollection)
    ['{AA4B805E-59D6-44E4-B571-EBE57B936906}']
    { Property Accessors }
    function Get_POSITIONMARKER(Index: Integer): IXMLPOSITIONMARKERType;
    { Methods & Properties }
    function Add: IXMLPOSITIONMARKERType;
    function Insert(const Index: Integer): IXMLPOSITIONMARKERType;
    property POSITIONMARKER[Index: Integer]: IXMLPOSITIONMARKERType read Get_POSITIONMARKER; default;
  end;

{ IXMLPOSITIONMARKERType }

  IXMLPOSITIONMARKERType = interface(IXMLNode)
    ['{E10FD481-29CB-43A5-A28A-7C2429BAF478}']
    { Property Accessors }
    function Get_ZoomFac: WideString;
    function Get_X: Integer;
    function Get_Y: Integer;
    procedure Set_ZoomFac(Value: WideString);
    procedure Set_X(Value: Integer);
    procedure Set_Y(Value: Integer);
    { Methods & Properties }
    property ZoomFac: WideString read Get_ZoomFac write Set_ZoomFac;
    property X: Integer read Get_X write Set_X;
    property Y: Integer read Get_Y write Set_Y;
  end;

{ IXMLMETADATAType }

  IXMLMETADATAType = interface(IXMLNode)
    ['{3C6DABB0-3EC7-4E44-8EC7-8A5E1B6C8F1A}']
    { Property Accessors }
    function Get_REGIONS: IXMLREGIONSType;
    function Get_TABLES: IXMLTABLESType;
    function Get_RELATIONS: IXMLRELATIONSType;
    function Get_NOTES: IXMLNOTESType;
    function Get_IMAGES: IXMLIMAGESType;
    { Methods & Properties }
    property REGIONS: IXMLREGIONSType read Get_REGIONS;
    property TABLES: IXMLTABLESType read Get_TABLES;
    property RELATIONS: IXMLRELATIONSType read Get_RELATIONS;
    property NOTES: IXMLNOTESType read Get_NOTES;
    property IMAGES: IXMLIMAGESType read Get_IMAGES;
  end;

{ IXMLREGIONSType }

  IXMLREGIONSType = interface(IXMLNodeCollection)
    ['{A0DC3396-10DD-415B-80BF-1132FAD4CC70}']
    { Property Accessors }
    function Get_REGION(Index: Integer): IXMLREGIONType;
    { Methods & Properties }
    function Add: IXMLREGIONType;
    function Insert(const Index: Integer): IXMLREGIONType;
    property REGION[Index: Integer]: IXMLREGIONType read Get_REGION; default;
  end;

{ IXMLREGIONType }

  IXMLREGIONType = interface(IXMLNode)
    ['{EFABEBA6-7BED-48CC-88E6-D86C247E38F6}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_RegionName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_Width: Integer;
    function Get_Height: Integer;
    function Get_RegionColor: Integer;
    function Get_TablePrefix: Integer;
    function Get_TableType: Integer;
    function Get_OverwriteTablePrefix: Integer;
    function Get_OverwriteTableType: Integer;
    function Get_Comments: WideString;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_RegionName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_Width(Value: Integer);
    procedure Set_Height(Value: Integer);
    procedure Set_RegionColor(Value: Integer);
    procedure Set_TablePrefix(Value: Integer);
    procedure Set_TableType(Value: Integer);
    procedure Set_OverwriteTablePrefix(Value: Integer);
    procedure Set_OverwriteTableType(Value: Integer);
    procedure Set_Comments(Value: WideString);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property RegionName: WideString read Get_RegionName write Set_RegionName;
    property XPos: Integer read Get_XPos write Set_XPos;
    property YPos: Integer read Get_YPos write Set_YPos;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property RegionColor: Integer read Get_RegionColor write Set_RegionColor;
    property TablePrefix: Integer read Get_TablePrefix write Set_TablePrefix;
    property TableType: Integer read Get_TableType write Set_TableType;
    property OverwriteTablePrefix: Integer read Get_OverwriteTablePrefix write Set_OverwriteTablePrefix;
    property OverwriteTableType: Integer read Get_OverwriteTableType write Set_OverwriteTableType;
    property Comments: WideString read Get_Comments write Set_Comments;
    property IsLinkedObject: Integer read Get_IsLinkedObject write Set_IsLinkedObject;
    property IDLinkedModel: WideString read Get_IDLinkedModel write Set_IDLinkedModel;
    property Obj_id_Linked: WideString read Get_Obj_id_Linked write Set_Obj_id_Linked;
  end;

{ IXMLTABLESType }

  IXMLTABLESType = interface(IXMLNodeCollection)
    ['{45F2EF9A-CE6E-477A-A4D2-82BDD3C70B7E}']
    { Property Accessors }
    function Get_TABLE(Index: Integer): IXMLTABLEType;
    { Methods & Properties }
    function Add: IXMLTABLEType;
    function Insert(const Index: Integer): IXMLTABLEType;
    property TABLE[Index: Integer]: IXMLTABLEType read Get_TABLE; default;
  end;

{ IXMLTABLEType }

  IXMLTABLEType = interface(IXMLNode)
    ['{B8FE24F5-191C-4C17-9239-D5D3E57D8A86}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_Tablename: WideString;
    function Get_PrevTableName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_TableType: Integer;
    function Get_TablePrefix: Integer;
    function Get_NmTable: Integer;
    function Get_Temporary: Integer;
    function Get_UseStandardInserts: Integer;
    function Get_StandardInserts: WideString;
    function Get_TableOptions: WideString;
    function Get_Comments: WideString;
    function Get_Collapsed: Integer;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    function Get_COLUMNS: IXMLCOLUMNSType;
    function Get_RELATIONS_START: IXMLRELATIONS_STARTType;
    function Get_RELATIONS_END: IXMLRELATIONS_ENDType;
    function Get_INDICES: IXMLINDICESType;
    procedure Set_ID(Value: Integer);
    procedure Set_Tablename(Value: WideString);
    procedure Set_PrevTableName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_TableType(Value: Integer);
    procedure Set_TablePrefix(Value: Integer);
    procedure Set_NmTable(Value: Integer);
    procedure Set_Temporary(Value: Integer);
    procedure Set_UseStandardInserts(Value: Integer);
    procedure Set_StandardInserts(Value: WideString);
    procedure Set_TableOptions(Value: WideString);
    procedure Set_Comments(Value: WideString);
    procedure Set_Collapsed(Value: Integer);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property Tablename: WideString read Get_Tablename write Set_Tablename;
    property PrevTableName: WideString read Get_PrevTableName write Set_PrevTableName;
    property XPos: Integer read Get_XPos write Set_XPos;
    property YPos: Integer read Get_YPos write Set_YPos;
    property TableType: Integer read Get_TableType write Set_TableType;
    property TablePrefix: Integer read Get_TablePrefix write Set_TablePrefix;
    property NmTable: Integer read Get_NmTable write Set_NmTable;
    property Temporary: Integer read Get_Temporary write Set_Temporary;
    property UseStandardInserts: Integer read Get_UseStandardInserts write Set_UseStandardInserts;
    property StandardInserts: WideString read Get_StandardInserts write Set_StandardInserts;
    property TableOptions: WideString read Get_TableOptions write Set_TableOptions;
    property Comments: WideString read Get_Comments write Set_Comments;
    property Collapsed: Integer read Get_Collapsed write Set_Collapsed;
    property IsLinkedObject: Integer read Get_IsLinkedObject write Set_IsLinkedObject;
    property IDLinkedModel: WideString read Get_IDLinkedModel write Set_IDLinkedModel;
    property Obj_id_Linked: WideString read Get_Obj_id_Linked write Set_Obj_id_Linked;
    property COLUMNS: IXMLCOLUMNSType read Get_COLUMNS;
    property RELATIONS_START: IXMLRELATIONS_STARTType read Get_RELATIONS_START;
    property RELATIONS_END: IXMLRELATIONS_ENDType read Get_RELATIONS_END;
    property INDICES: IXMLINDICESType read Get_INDICES;
  end;

{ IXMLCOLUMNSType }

  IXMLCOLUMNSType = interface(IXMLNodeCollection)
    ['{17A830DD-743B-4A60-9845-F7EF22CDB1C5}']
    { Property Accessors }
    function Get_COLUMN(Index: Integer): IXMLCOLUMNType;
    { Methods & Properties }
    function Add: IXMLCOLUMNType;
    function Insert(const Index: Integer): IXMLCOLUMNType;
    property COLUMN[Index: Integer]: IXMLCOLUMNType read Get_COLUMN; default;
  end;

{ IXMLCOLUMNType }

  IXMLCOLUMNType = interface(IXMLNode)
    ['{72C5C7E7-DC78-4BB2-AC51-9BF79E53917F}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_ColName: WideString;
    function Get_PrevColName: WideString;
    function Get_Pos: Integer;
    function Get_IdDatatype: Integer;
    function Get_DatatypeParams: WideString;
    function Get_Width: WideString;
    function Get_Prec: WideString;
    function Get_PrimaryKey: Integer;
    function Get_NotNull: Integer;
    function Get_AutoInc: Integer;
    function Get_IsForeignKey: Integer;
    function Get_DefaultValue: WideString;
    function Get_Comments: WideString;
    function Get_OPTIONSELECTED: IXMLOPTIONSELECTEDType;
    procedure Set_ID(Value: Integer);
    procedure Set_ColName(Value: WideString);
    procedure Set_PrevColName(Value: WideString);
    procedure Set_Pos(Value: Integer);
    procedure Set_IdDatatype(Value: Integer);
    procedure Set_DatatypeParams(Value: WideString);
    procedure Set_Width(Value: WideString);
    procedure Set_Prec(Value: WideString);
    procedure Set_PrimaryKey(Value: Integer);
    procedure Set_NotNull(Value: Integer);
    procedure Set_AutoInc(Value: Integer);
    procedure Set_IsForeignKey(Value: Integer);
    procedure Set_DefaultValue(Value: WideString);
    procedure Set_Comments(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property ColName: WideString read Get_ColName write Set_ColName;
    property PrevColName: WideString read Get_PrevColName write Set_PrevColName;
    property Pos: Integer read Get_Pos write Set_Pos;
    property IdDatatype: Integer read Get_IdDatatype write Set_IdDatatype;
    property DatatypeParams: WideString read Get_DatatypeParams write Set_DatatypeParams;
    property Width: WideString read Get_Width write Set_Width;
    property Prec: WideString read Get_Prec write Set_Prec;
    property PrimaryKey: Integer read Get_PrimaryKey write Set_PrimaryKey;
    property NotNull: Integer read Get_NotNull write Set_NotNull;
    property AutoInc: Integer read Get_AutoInc write Set_AutoInc;
    property IsForeignKey: Integer read Get_IsForeignKey write Set_IsForeignKey;
    property DefaultValue: WideString read Get_DefaultValue write Set_DefaultValue;
    property Comments: WideString read Get_Comments write Set_Comments;
    property OPTIONSELECTED: IXMLOPTIONSELECTEDType read Get_OPTIONSELECTED;
  end;

{ IXMLOPTIONSELECTEDType }

  IXMLOPTIONSELECTEDType = interface(IXMLNodeCollection)
    ['{5AB01ACC-24ED-4E5D-BC18-D0552135A96B}']
    { Property Accessors }
    function Get_OPTIONSELECT(Index: Integer): IXMLOPTIONSELECTType;
    { Methods & Properties }
    function Add: IXMLOPTIONSELECTType;
    function Insert(const Index: Integer): IXMLOPTIONSELECTType;
    property OPTIONSELECT[Index: Integer]: IXMLOPTIONSELECTType read Get_OPTIONSELECT; default;
  end;

{ IXMLOPTIONSELECTType }

  IXMLOPTIONSELECTType = interface(IXMLNode)
    ['{58851D97-E46A-420E-8B91-23A81854006A}']
    { Property Accessors }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
    { Methods & Properties }
    property Value: Integer read Get_Value write Set_Value;
  end;

{ IXMLRELATIONS_STARTType }

  IXMLRELATIONS_STARTType = interface(IXMLNodeCollection)
    ['{7D6DA579-46F2-43F2-86B2-52976E143DBD}']
    { Property Accessors }
    function Get_RELATION_START(Index: Integer): IXMLRELATION_STARTType;
    { Methods & Properties }
    function Add: IXMLRELATION_STARTType;
    function Insert(const Index: Integer): IXMLRELATION_STARTType;
    property RELATION_START[Index: Integer]: IXMLRELATION_STARTType read Get_RELATION_START; default;
  end;

{ IXMLRELATION_STARTType }

  IXMLRELATION_STARTType = interface(IXMLNode)
    ['{3C1BD9C0-CB4C-45D4-8209-FD3D043336CA}']
    { Property Accessors }
    function Get_ID: Integer;
    procedure Set_ID(Value: Integer);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
  end;

{ IXMLRELATIONS_ENDType }

  IXMLRELATIONS_ENDType = interface(IXMLNodeCollection)
    ['{1E22C6EE-DEDF-47A2-AAA8-4E06FC501B05}']
    { Property Accessors }
    function Get_RELATION_END(Index: Integer): IXMLRELATION_ENDType;
    { Methods & Properties }
    function Add: IXMLRELATION_ENDType;
    function Insert(const Index: Integer): IXMLRELATION_ENDType;
    property RELATION_END[Index: Integer]: IXMLRELATION_ENDType read Get_RELATION_END; default;
  end;

{ IXMLRELATION_ENDType }

  IXMLRELATION_ENDType = interface(IXMLNode)
    ['{073B02C3-8B24-41DA-A3C6-C99706DEDC2B}']
    { Property Accessors }
    function Get_ID: Integer;
    procedure Set_ID(Value: Integer);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
  end;

{ IXMLINDICESType }

  IXMLINDICESType = interface(IXMLNodeCollection)
    ['{32E38A45-E755-4723-AB0B-57F0520DA21A}']
    { Property Accessors }
    function Get_INDEX(Index: Integer): IXMLINDEXType;
    { Methods & Properties }
    function Add: IXMLINDEXType;
    function Insert(const Index: Integer): IXMLINDEXType;
    property INDEX[Index: Integer]: IXMLINDEXType read Get_INDEX; default;
  end;

{ IXMLINDEXType }

  IXMLINDEXType = interface(IXMLNode)
    ['{E27DEE48-969B-4073-BCB0-33EA1C4C1BC0}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_IndexName: WideString;
    function Get_IndexKind: Integer;
    function Get_FKRefDef_Obj_id: WideString;
    function Get_INDEXCOLUMNS: IXMLINDEXCOLUMNSType;
    procedure Set_ID(Value: Integer);
    procedure Set_IndexName(Value: WideString);
    procedure Set_IndexKind(Value: Integer);
    procedure Set_FKRefDef_Obj_id(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property IndexName: WideString read Get_IndexName write Set_IndexName;
    property IndexKind: Integer read Get_IndexKind write Set_IndexKind;
    property FKRefDef_Obj_id: WideString read Get_FKRefDef_Obj_id write Set_FKRefDef_Obj_id;
    property INDEXCOLUMNS: IXMLINDEXCOLUMNSType read Get_INDEXCOLUMNS;
  end;

{ IXMLINDEXCOLUMNSType }

  IXMLINDEXCOLUMNSType = interface(IXMLNodeCollection)
    ['{E031FD02-D97D-4618-B903-583A92FAFF91}']
    { Property Accessors }
    function Get_INDEXCOLUMN(Index: Integer): IXMLINDEXCOLUMNType;
    { Methods & Properties }
    function Add: IXMLINDEXCOLUMNType;
    function Insert(const Index: Integer): IXMLINDEXCOLUMNType;
    property INDEXCOLUMN[Index: Integer]: IXMLINDEXCOLUMNType read Get_INDEXCOLUMN; default;
  end;

{ IXMLINDEXCOLUMNType }

  IXMLINDEXCOLUMNType = interface(IXMLNode)
    ['{BB64B225-AE28-4C44-BEEC-03D2DEAC310A}']
    { Property Accessors }
    function Get_IdColumn: Integer;
    function Get_LengthParam: Integer;
    procedure Set_IdColumn(Value: Integer);
    procedure Set_LengthParam(Value: Integer);
    { Methods & Properties }
    property IdColumn: Integer read Get_IdColumn write Set_IdColumn;
    property LengthParam: Integer read Get_LengthParam write Set_LengthParam;
  end;

{ IXMLRELATIONSType }

  IXMLRELATIONSType = interface(IXMLNodeCollection)
    ['{C20E1318-3164-4D11-8CAD-5B1B7CE5B13D}']
    { Property Accessors }
    function Get_RELATION(Index: Integer): IXMLRELATIONType;
    { Methods & Properties }
    function Add: IXMLRELATIONType;
    function Insert(const Index: Integer): IXMLRELATIONType;
    property RELATION[Index: Integer]: IXMLRELATIONType read Get_RELATION; default;
  end;

{ IXMLRELATIONType }

  IXMLRELATIONType = interface(IXMLNode)
    ['{0CBD0D85-52FF-46FD-BD3C-9D15D0E9FCFB}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_RelationName: WideString;
    function Get_Kind: Integer;
    function Get_SrcTable: Integer;
    function Get_DestTable: Integer;
    function Get_FKFields: WideString;
    function Get_FKFieldsComments: WideString;
    function Get_RelDirection: Integer;
    function Get_MidOffset: WideString;
    function Get_OptionalStart: Integer;
    function Get_OptionalEnd: Integer;
    function Get_CaptionOffsetX: Integer;
    function Get_CaptionOffsetY: Integer;
    function Get_StartIntervalOffsetX: Integer;
    function Get_StartIntervalOffsetY: Integer;
    function Get_EndIntervalOffsetX: Integer;
    function Get_EndIntervalOffsetY: Integer;
    function Get_CreateRefDef: Integer;
    function Get_Invisible: Integer;
    function Get_RefDef: WideString;
    function Get_Comments: WideString;
    function Get_FKRefDefIndex_Obj_id: WideString;
    function Get_Splitted: Integer;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_RelationName(Value: WideString);
    procedure Set_Kind(Value: Integer);
    procedure Set_SrcTable(Value: Integer);
    procedure Set_DestTable(Value: Integer);
    procedure Set_FKFields(Value: WideString);
    procedure Set_FKFieldsComments(Value: WideString);
    procedure Set_RelDirection(Value: Integer);
    procedure Set_MidOffset(Value: WideString);
    procedure Set_OptionalStart(Value: Integer);
    procedure Set_OptionalEnd(Value: Integer);
    procedure Set_CaptionOffsetX(Value: Integer);
    procedure Set_CaptionOffsetY(Value: Integer);
    procedure Set_StartIntervalOffsetX(Value: Integer);
    procedure Set_StartIntervalOffsetY(Value: Integer);
    procedure Set_EndIntervalOffsetX(Value: Integer);
    procedure Set_EndIntervalOffsetY(Value: Integer);
    procedure Set_CreateRefDef(Value: Integer);
    procedure Set_Invisible(Value: Integer);
    procedure Set_RefDef(Value: WideString);
    procedure Set_Comments(Value: WideString);
    procedure Set_FKRefDefIndex_Obj_id(Value: WideString);
    procedure Set_Splitted(Value: Integer);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property RelationName: WideString read Get_RelationName write Set_RelationName;
    property Kind: Integer read Get_Kind write Set_Kind;
    property SrcTable: Integer read Get_SrcTable write Set_SrcTable;
    property DestTable: Integer read Get_DestTable write Set_DestTable;
    property FKFields: WideString read Get_FKFields write Set_FKFields;
    property FKFieldsComments: WideString read Get_FKFieldsComments write Set_FKFieldsComments;
    property RelDirection: Integer read Get_RelDirection write Set_RelDirection;
    property MidOffset: WideString read Get_MidOffset write Set_MidOffset;
    property OptionalStart: Integer read Get_OptionalStart write Set_OptionalStart;
    property OptionalEnd: Integer read Get_OptionalEnd write Set_OptionalEnd;
    property CaptionOffsetX: Integer read Get_CaptionOffsetX write Set_CaptionOffsetX;
    property CaptionOffsetY: Integer read Get_CaptionOffsetY write Set_CaptionOffsetY;
    property StartIntervalOffsetX: Integer read Get_StartIntervalOffsetX write Set_StartIntervalOffsetX;
    property StartIntervalOffsetY: Integer read Get_StartIntervalOffsetY write Set_StartIntervalOffsetY;
    property EndIntervalOffsetX: Integer read Get_EndIntervalOffsetX write Set_EndIntervalOffsetX;
    property EndIntervalOffsetY: Integer read Get_EndIntervalOffsetY write Set_EndIntervalOffsetY;
    property CreateRefDef: Integer read Get_CreateRefDef write Set_CreateRefDef;
    property Invisible: Integer read Get_Invisible write Set_Invisible;
    property RefDef: WideString read Get_RefDef write Set_RefDef;
    property Comments: WideString read Get_Comments write Set_Comments;
    property FKRefDefIndex_Obj_id: WideString read Get_FKRefDefIndex_Obj_id write Set_FKRefDefIndex_Obj_id;
    property Splitted: Integer read Get_Splitted write Set_Splitted;
    property IsLinkedObject: Integer read Get_IsLinkedObject write Set_IsLinkedObject;
    property IDLinkedModel: WideString read Get_IDLinkedModel write Set_IDLinkedModel;
    property Obj_id_Linked: WideString read Get_Obj_id_Linked write Set_Obj_id_Linked;
  end;

{ IXMLNOTESType }

  IXMLNOTESType = interface(IXMLNodeCollection)
    ['{2D05F933-A765-4FDA-A504-F1A89BAE1BF0}']
    { Property Accessors }
    function Get_NOTE(Index: Integer): IXMLNOTEType;
    { Methods & Properties }
    function Add: IXMLNOTEType;
    function Insert(const Index: Integer): IXMLNOTEType;
    property NOTE[Index: Integer]: IXMLNOTEType read Get_NOTE; default;
  end;

{ IXMLNOTEType }

  IXMLNOTEType = interface(IXMLNode)
    ['{65E45F06-E6F7-4123-83E9-DEFAE2E8FF13}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_NoteName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_NoteText: WideString;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_NoteName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_NoteText(Value: WideString);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property NoteName: WideString read Get_NoteName write Set_NoteName;
    property XPos: Integer read Get_XPos write Set_XPos;
    property YPos: Integer read Get_YPos write Set_YPos;
    property NoteText: WideString read Get_NoteText write Set_NoteText;
    property IsLinkedObject: Integer read Get_IsLinkedObject write Set_IsLinkedObject;
    property IDLinkedModel: WideString read Get_IDLinkedModel write Set_IDLinkedModel;
    property Obj_id_Linked: WideString read Get_Obj_id_Linked write Set_Obj_id_Linked;
  end;

{ IXMLIMAGESType }

  IXMLIMAGESType = interface(IXMLNodeCollection)
    ['{97C3D0C8-E8A4-4E1A-A280-C88876613BEE}']
    { Property Accessors }
    function Get_IMAGE(Index: Integer): IXMLIMAGEType;
    { Methods & Properties }
    function Add: IXMLIMAGEType;
    function Insert(const Index: Integer): IXMLIMAGEType;
    property IMAGE[Index: Integer]: IXMLIMAGEType read Get_IMAGE; default;
  end;

{ IXMLIMAGEType }

  IXMLIMAGEType = interface(IXMLNode)
    ['{EC5E61F1-D275-42C9-B7B8-1FE3F80BB15C}']
    { Property Accessors }
    function Get_ID: Integer;
    function Get_ImageName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_Width: Integer;
    function Get_Height: Integer;
    function Get_StrechImg: Integer;
    function Get_ImgWidth: Integer;
    function Get_ImgHeight: Integer;
    function Get_ImgFormat: WideString;
    function Get_ImgData: WideString;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_ImageName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_Width(Value: Integer);
    procedure Set_Height(Value: Integer);
    procedure Set_StrechImg(Value: Integer);
    procedure Set_ImgWidth(Value: Integer);
    procedure Set_ImgHeight(Value: Integer);
    procedure Set_ImgFormat(Value: WideString);
    procedure Set_ImgData(Value: WideString);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
    { Methods & Properties }
    property ID: Integer read Get_ID write Set_ID;
    property ImageName: WideString read Get_ImageName write Set_ImageName;
    property XPos: Integer read Get_XPos write Set_XPos;
    property YPos: Integer read Get_YPos write Set_YPos;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property StrechImg: Integer read Get_StrechImg write Set_StrechImg;
    property ImgWidth: Integer read Get_ImgWidth write Set_ImgWidth;
    property ImgHeight: Integer read Get_ImgHeight write Set_ImgHeight;
    property ImgFormat: WideString read Get_ImgFormat write Set_ImgFormat;
    property ImgData: WideString read Get_ImgData write Set_ImgData;
    property IsLinkedObject: Integer read Get_IsLinkedObject write Set_IsLinkedObject;
    property IDLinkedModel: WideString read Get_IDLinkedModel write Set_IDLinkedModel;
    property Obj_id_Linked: WideString read Get_Obj_id_Linked write Set_Obj_id_Linked;
  end;

{ IXMLPLUGINDATAType }

  IXMLPLUGINDATAType = interface(IXMLNode)
    ['{CEB12526-5E2A-4509-9B4B-78AD9373E399}']
    { Property Accessors }
    function Get_PLUGINDATARECORDS: IXMLPLUGINDATARECORDSType;
    { Methods & Properties }
    property PLUGINDATARECORDS: IXMLPLUGINDATARECORDSType read Get_PLUGINDATARECORDS;
  end;

{ IXMLPLUGINDATARECORDSType }

  IXMLPLUGINDATARECORDSType = interface(IXMLNodeCollection)
    ['{508299EE-8FB3-4835-870B-CBBE144DD392}']
    { Property Accessors }
    function Get_PLUGINDATARECORD(Index: Integer): IXMLPLUGINDATARECORDType;
    { Methods & Properties }
    function Add: IXMLPLUGINDATARECORDType;
    function Insert(const Index: Integer): IXMLPLUGINDATARECORDType;
    property PLUGINDATARECORD[Index: Integer]: IXMLPLUGINDATARECORDType read Get_PLUGINDATARECORD; default;
  end;

{ IXMLPLUGINDATARECORDType }

  IXMLPLUGINDATARECORDType = interface(IXMLNode)
    ['{2DD7D2C1-07E8-4137-A232-62C835C37637}']
    { Property Accessors }
    function Get_PluginName: WideString;
    function Get_Obj_id: WideString;
    function Get_DataValue: WideString;
    function Get_PLUGINDATAPARAMS: IXMLPLUGINDATAPARAMSType;
    procedure Set_PluginName(Value: WideString);
    procedure Set_Obj_id(Value: WideString);
    procedure Set_DataValue(Value: WideString);
    { Methods & Properties }
    property PluginName: WideString read Get_PluginName write Set_PluginName;
    property Obj_id: WideString read Get_Obj_id write Set_Obj_id;
    property DataValue: WideString read Get_DataValue write Set_DataValue;
    property PLUGINDATAPARAMS: IXMLPLUGINDATAPARAMSType read Get_PLUGINDATAPARAMS;
  end;

{ IXMLPLUGINDATAPARAMSType }

  IXMLPLUGINDATAPARAMSType = interface(IXMLNodeCollection)
    ['{16CF4A7B-E2B5-4715-B153-B281F89B892D}']
    { Property Accessors }
    function Get_PLUGINDATAPARAM(Index: Integer): IXMLPLUGINDATAPARAMType;
    { Methods & Properties }
    function Add: IXMLPLUGINDATAPARAMType;
    function Insert(const Index: Integer): IXMLPLUGINDATAPARAMType;
    property PLUGINDATAPARAM[Index: Integer]: IXMLPLUGINDATAPARAMType read Get_PLUGINDATAPARAM; default;
  end;

{ IXMLPLUGINDATAPARAMType }

  IXMLPLUGINDATAPARAMType = interface(IXMLNode)
    ['{D327A6CC-207C-48A3-A5E2-2C580A7E8759}']
    { Property Accessors }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
    { Methods & Properties }
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLQUERYDATAType }

  IXMLQUERYDATAType = interface(IXMLNode)
    ['{8A3BB851-CC36-41E9-B518-5F9AB88A0396}']
    { Property Accessors }
    function Get_QUERYRECORDS: IXMLQUERYRECORDSType;
    { Methods & Properties }
    property QUERYRECORDS: IXMLQUERYRECORDSType read Get_QUERYRECORDS;
  end;

{ IXMLQUERYRECORDSType }

  IXMLQUERYRECORDSType = interface(IXMLNodeCollection)
    ['{9F0720B7-682A-4E79-9720-87173AA5A091}']
    { Property Accessors }
    function Get_QUERYRECORD(Index: Integer): IXMLQUERYRECORDType;
    { Methods & Properties }
    function Add: IXMLQUERYRECORDType;
    function Insert(const Index: Integer): IXMLQUERYRECORDType;
    property QUERYRECORD[Index: Integer]: IXMLQUERYRECORDType read Get_QUERYRECORD; default;
  end;

{ IXMLQUERYRECORDType }

  IXMLQUERYRECORDType = interface(IXMLNode)
    ['{C9E8FDE3-71A5-4803-81AC-727292A0E3E4}']
    { Property Accessors }
    function Get_SQLCmdType: Integer;
    function Get_StoredPosition: WideString;
    function Get_SQLText: WideString;
    procedure Set_SQLCmdType(Value: Integer);
    procedure Set_StoredPosition(Value: WideString);
    procedure Set_SQLText(Value: WideString);
    { Methods & Properties }
    property SQLCmdType: Integer read Get_SQLCmdType write Set_SQLCmdType;
    property StoredPosition: WideString read Get_StoredPosition write Set_StoredPosition;
    property SQLText: WideString read Get_SQLText write Set_SQLText;
  end;

{ IXMLLINKEDMODELSType }

  IXMLLINKEDMODELSType = interface(IXMLNodeCollection)
    ['{6DE2D4A1-EBFB-4FB3-BF2C-6FE2098C1234}']
    { Property Accessors }
    function Get_LINKEDMODEL(Index: Integer): IXMLLINKEDMODELType;
    { Methods & Properties }
    function Add: IXMLLINKEDMODELType;
    function Insert(const Index: Integer): IXMLLINKEDMODELType;
    property LINKEDMODEL[Index: Integer]: IXMLLINKEDMODELType read Get_LINKEDMODEL; default;
  end;

{ IXMLLINKEDMODELType }

  IXMLLINKEDMODELType = interface(IXMLNode)
    ['{13A029BF-BD5E-4425-B934-5ABBE103BF6B}']
    { Property Accessors }
    function Get_IDLinkedModel: Integer;
    function Get_ModelName: WideString;
    function Get_IDModel: Integer;
    function Get_IsStoredInDB: Integer;
    function Get_ModelFilename: WideString;
    function Get_DriverName: WideString;
    function Get_DBConnName: WideString;
    function Get_HostCaption: WideString;
    function Get_HostName: WideString;
    function Get_Database: WideString;
    function Get_User: WideString;
    procedure Set_IDLinkedModel(Value: Integer);
    procedure Set_ModelName(Value: WideString);
    procedure Set_IDModel(Value: Integer);
    procedure Set_IsStoredInDB(Value: Integer);
    procedure Set_ModelFilename(Value: WideString);
    procedure Set_DriverName(Value: WideString);
    procedure Set_DBConnName(Value: WideString);
    procedure Set_HostCaption(Value: WideString);
    procedure Set_HostName(Value: WideString);
    procedure Set_Database(Value: WideString);
    procedure Set_User(Value: WideString);
    { Methods & Properties }
    property IDLinkedModel: Integer read Get_IDLinkedModel write Set_IDLinkedModel;
    property ModelName: WideString read Get_ModelName write Set_ModelName;
    property IDModel: Integer read Get_IDModel write Set_IDModel;
    property IsStoredInDB: Integer read Get_IsStoredInDB write Set_IsStoredInDB;
    property ModelFilename: WideString read Get_ModelFilename write Set_ModelFilename;
    property DriverName: WideString read Get_DriverName write Set_DriverName;
    property DBConnName: WideString read Get_DBConnName write Set_DBConnName;
    property HostCaption: WideString read Get_HostCaption write Set_HostCaption;
    property HostName: WideString read Get_HostName write Set_HostName;
    property Database: WideString read Get_Database write Set_Database;
    property User: WideString read Get_User write Set_User;
  end;

{ Forward Decls }

  TXMLDBMODELType = class;
  TXMLSETTINGSType = class;
  TXMLGLOBALSETTINGSType = class;
  TXMLDATATYPEGROUPSType = class;
  TXMLDATATYPEGROUPType = class;
  TXMLDATATYPESType = class;
  TXMLDATATYPEType = class;
  TXMLPARAMSType = class;
  TXMLPARAMType = class;
  TXMLOPTIONSType = class;
  TXMLOPTIONType = class;
  TXMLCOMMON_DATATYPESType = class;
  TXMLCOMMON_DATATYPEType = class;
  TXMLTABLEPREFIXESType = class;
  TXMLTABLEPREFIXType = class;
  TXMLREGIONCOLORSType = class;
  TXMLREGIONCOLORType = class;
  TXMLPOSITIONMARKERSType = class;
  TXMLPOSITIONMARKERType = class;
  TXMLMETADATAType = class;
  TXMLREGIONSType = class;
  TXMLREGIONType = class;
  TXMLTABLESType = class;
  TXMLTABLEType = class;
  TXMLCOLUMNSType = class;
  TXMLCOLUMNType = class;
  TXMLOPTIONSELECTEDType = class;
  TXMLOPTIONSELECTType = class;
  TXMLRELATIONS_STARTType = class;
  TXMLRELATION_STARTType = class;
  TXMLRELATIONS_ENDType = class;
  TXMLRELATION_ENDType = class;
  TXMLINDICESType = class;
  TXMLINDEXType = class;
  TXMLINDEXCOLUMNSType = class;
  TXMLINDEXCOLUMNType = class;
  TXMLRELATIONSType = class;
  TXMLRELATIONType = class;
  TXMLNOTESType = class;
  TXMLNOTEType = class;
  TXMLIMAGESType = class;
  TXMLIMAGEType = class;
  TXMLPLUGINDATAType = class;
  TXMLPLUGINDATARECORDSType = class;
  TXMLPLUGINDATARECORDType = class;
  TXMLPLUGINDATAPARAMSType = class;
  TXMLPLUGINDATAPARAMType = class;
  TXMLQUERYDATAType = class;
  TXMLQUERYRECORDSType = class;
  TXMLQUERYRECORDType = class;
  TXMLLINKEDMODELSType = class;
  TXMLLINKEDMODELType = class;

{ TXMLDBMODELType }

  TXMLDBMODELType = class(TXMLNode, IXMLDBMODELType)
  protected
    { IXMLDBMODELType }
    function Get_Version: WideString;
    function Get_SETTINGS: IXMLSETTINGSType;
    function Get_METADATA: IXMLMETADATAType;
    function Get_PLUGINDATA: IXMLPLUGINDATAType;
    function Get_QUERYDATA: IXMLQUERYDATAType;
    function Get_LINKEDMODELS: IXMLLINKEDMODELSType;
    procedure Set_Version(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSETTINGSType }

  TXMLSETTINGSType = class(TXMLNode, IXMLSETTINGSType)
  protected
    { IXMLSETTINGSType }
    function Get_GLOBALSETTINGS: IXMLGLOBALSETTINGSType;
    function Get_DATATYPEGROUPS: IXMLDATATYPEGROUPSType;
    function Get_DATATYPES: IXMLDATATYPESType;
    function Get_COMMON_DATATYPES: IXMLCOMMON_DATATYPESType;
    function Get_TABLEPREFIXES: IXMLTABLEPREFIXESType;
    function Get_REGIONCOLORS: IXMLREGIONCOLORSType;
    function Get_POSITIONMARKERS: IXMLPOSITIONMARKERSType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGLOBALSETTINGSType }

  TXMLGLOBALSETTINGSType = class(TXMLNode, IXMLGLOBALSETTINGSType)
  protected
    { IXMLGLOBALSETTINGSType }
    function Get_ModelName: WideString;
    function Get_IDModel: Integer;
    function Get_IDVersion: Integer;
    function Get_VersionStr: WideString;
    function Get_Comments: WideString;
    function Get_UseVersionHistroy: Integer;
    function Get_AutoIncVersion: Integer;
    function Get_DatabaseType: WideString;
    function Get_ZoomFac: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_DefaultDataType: Integer;
    function Get_DefaultTablePrefix: Integer;
    function Get_DefSaveDBConn: WideString;
    function Get_DefSyncDBConn: WideString;
    function Get_DefQueryDBConn: WideString;
    function Get_Printer: WideString;
    function Get_HPageCount: WideString;
    function Get_PageAspectRatio: WideString;
    function Get_PageOrientation: Integer;
    function Get_PageFormat: WideString;
    function Get_SelectedPages: WideString;
    function Get_UsePositionGrid: Integer;
    function Get_PositionGridX: Integer;
    function Get_PositionGridY: Integer;
    function Get_TableNameInRefs: Integer;
    function Get_DefaultTableType: Integer;
    function Get_ActivateRefDefForNewRelations: Integer;
    function Get_FKPrefix: WideString;
    function Get_FKPostfix: WideString;
    function Get_CreateFKRefDefIndex: Integer;
    function Get_DBQuoteCharacter: WideString;
    function Get_CreateSQLforLinkedObjects: Integer;
    function Get_DefModelFont: WideString;
    function Get_CanvasWidth: Integer;
    function Get_CanvasHeight: Integer;
    procedure Set_ModelName(Value: WideString);
    procedure Set_IDModel(Value: Integer);
    procedure Set_IDVersion(Value: Integer);
    procedure Set_VersionStr(Value: WideString);
    procedure Set_Comments(Value: WideString);
    procedure Set_UseVersionHistroy(Value: Integer);
    procedure Set_AutoIncVersion(Value: Integer);
    procedure Set_DatabaseType(Value: WideString);
    procedure Set_ZoomFac(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_DefaultDataType(Value: Integer);
    procedure Set_DefaultTablePrefix(Value: Integer);
    procedure Set_DefSaveDBConn(Value: WideString);
    procedure Set_DefSyncDBConn(Value: WideString);
    procedure Set_DefQueryDBConn(Value: WideString);
    procedure Set_Printer(Value: WideString);
    procedure Set_HPageCount(Value: WideString);
    procedure Set_PageAspectRatio(Value: WideString);
    procedure Set_PageOrientation(Value: Integer);
    procedure Set_PageFormat(Value: WideString);
    procedure Set_SelectedPages(Value: WideString);
    procedure Set_UsePositionGrid(Value: Integer);
    procedure Set_PositionGridX(Value: Integer);
    procedure Set_PositionGridY(Value: Integer);
    procedure Set_TableNameInRefs(Value: Integer);
    procedure Set_DefaultTableType(Value: Integer);
    procedure Set_ActivateRefDefForNewRelations(Value: Integer);
    procedure Set_FKPrefix(Value: WideString);
    procedure Set_FKPostfix(Value: WideString);
    procedure Set_CreateFKRefDefIndex(Value: Integer);
    procedure Set_DBQuoteCharacter(Value: WideString);
    procedure Set_CreateSQLforLinkedObjects(Value: Integer);
    procedure Set_DefModelFont(Value: WideString);
    procedure Set_CanvasWidth(Value: Integer);
    procedure Set_CanvasHeight(Value: Integer);
  end;

{ TXMLDATATYPEGROUPSType }

  TXMLDATATYPEGROUPSType = class(TXMLNodeCollection, IXMLDATATYPEGROUPSType)
  protected
    { IXMLDATATYPEGROUPSType }
    function Get_DATATYPEGROUP(Index: Integer): IXMLDATATYPEGROUPType;
    function Add: IXMLDATATYPEGROUPType;
    function Insert(const Index: Integer): IXMLDATATYPEGROUPType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDATATYPEGROUPType }

  TXMLDATATYPEGROUPType = class(TXMLNode, IXMLDATATYPEGROUPType)
  protected
    { IXMLDATATYPEGROUPType }
    function Get_Name: WideString;
    function Get_Icon: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Icon(Value: Integer);
  end;

{ TXMLDATATYPESType }

  TXMLDATATYPESType = class(TXMLNodeCollection, IXMLDATATYPESType)
  protected
    { IXMLDATATYPESType }
    function Get_DATATYPE(Index: Integer): IXMLDATATYPEType;
    function Add: IXMLDATATYPEType;
    function Insert(const Index: Integer): IXMLDATATYPEType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDATATYPEType }

  TXMLDATATYPEType = class(TXMLNode, IXMLDATATYPEType)
  protected
    { IXMLDATATYPEType }
    function Get_ID: Integer;
    function Get_IDGroup: Integer;
    function Get_TypeName: WideString;
    function Get_Description: WideString;
    function Get_ParamCount: Integer;
    function Get_OptionCount: Integer;
    function Get_ParamRequired: Integer;
    function Get_EditParamsAsString: Integer;
    function Get_SynonymGroup: Integer;
    function Get_PhysicalMapping: Integer;
    function Get_PhysicalTypeName: WideString;
    function Get_PARAMS: IXMLPARAMSType;
    function Get_OPTIONS: IXMLOPTIONSType;
    procedure Set_ID(Value: Integer);
    procedure Set_IDGroup(Value: Integer);
    procedure Set_TypeName(Value: WideString);
    procedure Set_Description(Value: WideString);
    procedure Set_ParamCount(Value: Integer);
    procedure Set_OptionCount(Value: Integer);
    procedure Set_ParamRequired(Value: Integer);
    procedure Set_EditParamsAsString(Value: Integer);
    procedure Set_SynonymGroup(Value: Integer);
    procedure Set_PhysicalMapping(Value: Integer);
    procedure Set_PhysicalTypeName(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPARAMSType }

  TXMLPARAMSType = class(TXMLNodeCollection, IXMLPARAMSType)
  protected
    { IXMLPARAMSType }
    function Get_PARAM(Index: Integer): IXMLPARAMType;
    function Add: IXMLPARAMType;
    function Insert(const Index: Integer): IXMLPARAMType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPARAMType }

  TXMLPARAMType = class(TXMLNode, IXMLPARAMType)
  protected
    { IXMLPARAMType }
    function Get_Name: WideString;
    procedure Set_Name(Value: WideString);
  end;

{ TXMLOPTIONSType }

  TXMLOPTIONSType = class(TXMLNodeCollection, IXMLOPTIONSType)
  protected
    { IXMLOPTIONSType }
    function Get_OPTION(Index: Integer): IXMLOPTIONType;
    function Add: IXMLOPTIONType;
    function Insert(const Index: Integer): IXMLOPTIONType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLOPTIONType }

  TXMLOPTIONType = class(TXMLNode, IXMLOPTIONType)
  protected
    { IXMLOPTIONType }
    function Get_Name: WideString;
    function Get_Default: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Default(Value: Integer);
  end;

{ TXMLCOMMON_DATATYPESType }

  TXMLCOMMON_DATATYPESType = class(TXMLNodeCollection, IXMLCOMMON_DATATYPESType)
  protected
    { IXMLCOMMON_DATATYPESType }
    function Get_COMMON_DATATYPE(Index: Integer): IXMLCOMMON_DATATYPEType;
    function Add: IXMLCOMMON_DATATYPEType;
    function Insert(const Index: Integer): IXMLCOMMON_DATATYPEType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCOMMON_DATATYPEType }

  TXMLCOMMON_DATATYPEType = class(TXMLNode, IXMLCOMMON_DATATYPEType)
  protected
    { IXMLCOMMON_DATATYPEType }
    function Get_ID: Integer;
    procedure Set_ID(Value: Integer);
  end;

{ TXMLTABLEPREFIXESType }

  TXMLTABLEPREFIXESType = class(TXMLNodeCollection, IXMLTABLEPREFIXESType)
  protected
    { IXMLTABLEPREFIXESType }
    function Get_TABLEPREFIX(Index: Integer): IXMLTABLEPREFIXType;
    function Add: IXMLTABLEPREFIXType;
    function Insert(const Index: Integer): IXMLTABLEPREFIXType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTABLEPREFIXType }

  TXMLTABLEPREFIXType = class(TXMLNode, IXMLTABLEPREFIXType)
  protected
    { IXMLTABLEPREFIXType }
    function Get_Name: WideString;
    procedure Set_Name(Value: WideString);
  end;

{ TXMLREGIONCOLORSType }

  TXMLREGIONCOLORSType = class(TXMLNodeCollection, IXMLREGIONCOLORSType)
  protected
    { IXMLREGIONCOLORSType }
    function Get_REGIONCOLOR(Index: Integer): IXMLREGIONCOLORType;
    function Add: IXMLREGIONCOLORType;
    function Insert(const Index: Integer): IXMLREGIONCOLORType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLREGIONCOLORType }

  TXMLREGIONCOLORType = class(TXMLNode, IXMLREGIONCOLORType)
  protected
    { IXMLREGIONCOLORType }
    function Get_Color: WideString;
    procedure Set_Color(Value: WideString);
  end;

{ TXMLPOSITIONMARKERSType }

  TXMLPOSITIONMARKERSType = class(TXMLNodeCollection, IXMLPOSITIONMARKERSType)
  protected
    { IXMLPOSITIONMARKERSType }
    function Get_POSITIONMARKER(Index: Integer): IXMLPOSITIONMARKERType;
    function Add: IXMLPOSITIONMARKERType;
    function Insert(const Index: Integer): IXMLPOSITIONMARKERType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPOSITIONMARKERType }

  TXMLPOSITIONMARKERType = class(TXMLNode, IXMLPOSITIONMARKERType)
  protected
    { IXMLPOSITIONMARKERType }
    function Get_ZoomFac: WideString;
    function Get_X: Integer;
    function Get_Y: Integer;
    procedure Set_ZoomFac(Value: WideString);
    procedure Set_X(Value: Integer);
    procedure Set_Y(Value: Integer);
  end;

{ TXMLMETADATAType }

  TXMLMETADATAType = class(TXMLNode, IXMLMETADATAType)
  protected
    { IXMLMETADATAType }
    function Get_REGIONS: IXMLREGIONSType;
    function Get_TABLES: IXMLTABLESType;
    function Get_RELATIONS: IXMLRELATIONSType;
    function Get_NOTES: IXMLNOTESType;
    function Get_IMAGES: IXMLIMAGESType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLREGIONSType }

  TXMLREGIONSType = class(TXMLNodeCollection, IXMLREGIONSType)
  protected
    { IXMLREGIONSType }
    function Get_REGION(Index: Integer): IXMLREGIONType;
    function Add: IXMLREGIONType;
    function Insert(const Index: Integer): IXMLREGIONType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLREGIONType }

  TXMLREGIONType = class(TXMLNode, IXMLREGIONType)
  protected
    { IXMLREGIONType }
    function Get_ID: Integer;
    function Get_RegionName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_Width: Integer;
    function Get_Height: Integer;
    function Get_RegionColor: Integer;
    function Get_TablePrefix: Integer;
    function Get_TableType: Integer;
    function Get_OverwriteTablePrefix: Integer;
    function Get_OverwriteTableType: Integer;
    function Get_Comments: WideString;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_RegionName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_Width(Value: Integer);
    procedure Set_Height(Value: Integer);
    procedure Set_RegionColor(Value: Integer);
    procedure Set_TablePrefix(Value: Integer);
    procedure Set_TableType(Value: Integer);
    procedure Set_OverwriteTablePrefix(Value: Integer);
    procedure Set_OverwriteTableType(Value: Integer);
    procedure Set_Comments(Value: WideString);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
  end;

{ TXMLTABLESType }

  TXMLTABLESType = class(TXMLNodeCollection, IXMLTABLESType)
  protected
    { IXMLTABLESType }
    function Get_TABLE(Index: Integer): IXMLTABLEType;
    function Add: IXMLTABLEType;
    function Insert(const Index: Integer): IXMLTABLEType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTABLEType }

  TXMLTABLEType = class(TXMLNode, IXMLTABLEType)
  protected
    { IXMLTABLEType }
    function Get_ID: Integer;
    function Get_Tablename: WideString;
    function Get_PrevTableName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_TableType: Integer;
    function Get_TablePrefix: Integer;
    function Get_NmTable: Integer;
    function Get_Temporary: Integer;
    function Get_UseStandardInserts: Integer;
    function Get_StandardInserts: WideString;
    function Get_TableOptions: WideString;
    function Get_Comments: WideString;
    function Get_Collapsed: Integer;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    function Get_COLUMNS: IXMLCOLUMNSType;
    function Get_RELATIONS_START: IXMLRELATIONS_STARTType;
    function Get_RELATIONS_END: IXMLRELATIONS_ENDType;
    function Get_INDICES: IXMLINDICESType;
    procedure Set_ID(Value: Integer);
    procedure Set_Tablename(Value: WideString);
    procedure Set_PrevTableName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_TableType(Value: Integer);
    procedure Set_TablePrefix(Value: Integer);
    procedure Set_NmTable(Value: Integer);
    procedure Set_Temporary(Value: Integer);
    procedure Set_UseStandardInserts(Value: Integer);
    procedure Set_StandardInserts(Value: WideString);
    procedure Set_TableOptions(Value: WideString);
    procedure Set_Comments(Value: WideString);
    procedure Set_Collapsed(Value: Integer);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCOLUMNSType }

  TXMLCOLUMNSType = class(TXMLNodeCollection, IXMLCOLUMNSType)
  protected
    { IXMLCOLUMNSType }
    function Get_COLUMN(Index: Integer): IXMLCOLUMNType;
    function Add: IXMLCOLUMNType;
    function Insert(const Index: Integer): IXMLCOLUMNType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCOLUMNType }

  TXMLCOLUMNType = class(TXMLNode, IXMLCOLUMNType)
  protected
    { IXMLCOLUMNType }
    function Get_ID: Integer;
    function Get_ColName: WideString;
    function Get_PrevColName: WideString;
    function Get_Pos: Integer;
    function Get_IdDatatype: Integer;
    function Get_DatatypeParams: WideString;
    function Get_Width: WideString;
    function Get_Prec: WideString;
    function Get_PrimaryKey: Integer;
    function Get_NotNull: Integer;
    function Get_AutoInc: Integer;
    function Get_IsForeignKey: Integer;
    function Get_DefaultValue: WideString;
    function Get_Comments: WideString;
    function Get_OPTIONSELECTED: IXMLOPTIONSELECTEDType;
    procedure Set_ID(Value: Integer);
    procedure Set_ColName(Value: WideString);
    procedure Set_PrevColName(Value: WideString);
    procedure Set_Pos(Value: Integer);
    procedure Set_IdDatatype(Value: Integer);
    procedure Set_DatatypeParams(Value: WideString);
    procedure Set_Width(Value: WideString);
    procedure Set_Prec(Value: WideString);
    procedure Set_PrimaryKey(Value: Integer);
    procedure Set_NotNull(Value: Integer);
    procedure Set_AutoInc(Value: Integer);
    procedure Set_IsForeignKey(Value: Integer);
    procedure Set_DefaultValue(Value: WideString);
    procedure Set_Comments(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLOPTIONSELECTEDType }

  TXMLOPTIONSELECTEDType = class(TXMLNodeCollection, IXMLOPTIONSELECTEDType)
  protected
    { IXMLOPTIONSELECTEDType }
    function Get_OPTIONSELECT(Index: Integer): IXMLOPTIONSELECTType;
    function Add: IXMLOPTIONSELECTType;
    function Insert(const Index: Integer): IXMLOPTIONSELECTType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLOPTIONSELECTType }

  TXMLOPTIONSELECTType = class(TXMLNode, IXMLOPTIONSELECTType)
  protected
    { IXMLOPTIONSELECTType }
    function Get_Value: Integer;
    procedure Set_Value(Value: Integer);
  end;

{ TXMLRELATIONS_STARTType }

  TXMLRELATIONS_STARTType = class(TXMLNodeCollection, IXMLRELATIONS_STARTType)
  protected
    { IXMLRELATIONS_STARTType }
    function Get_RELATION_START(Index: Integer): IXMLRELATION_STARTType;
    function Add: IXMLRELATION_STARTType;
    function Insert(const Index: Integer): IXMLRELATION_STARTType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRELATION_STARTType }

  TXMLRELATION_STARTType = class(TXMLNode, IXMLRELATION_STARTType)
  protected
    { IXMLRELATION_STARTType }
    function Get_ID: Integer;
    procedure Set_ID(Value: Integer);
  end;

{ TXMLRELATIONS_ENDType }

  TXMLRELATIONS_ENDType = class(TXMLNodeCollection, IXMLRELATIONS_ENDType)
  protected
    { IXMLRELATIONS_ENDType }
    function Get_RELATION_END(Index: Integer): IXMLRELATION_ENDType;
    function Add: IXMLRELATION_ENDType;
    function Insert(const Index: Integer): IXMLRELATION_ENDType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRELATION_ENDType }

  TXMLRELATION_ENDType = class(TXMLNode, IXMLRELATION_ENDType)
  protected
    { IXMLRELATION_ENDType }
    function Get_ID: Integer;
    procedure Set_ID(Value: Integer);
  end;

{ TXMLINDICESType }

  TXMLINDICESType = class(TXMLNodeCollection, IXMLINDICESType)
  protected
    { IXMLINDICESType }
    function Get_INDEX(Index: Integer): IXMLINDEXType;
    function Add: IXMLINDEXType;
    function Insert(const Index: Integer): IXMLINDEXType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLINDEXType }

  TXMLINDEXType = class(TXMLNode, IXMLINDEXType)
  protected
    { IXMLINDEXType }
    function Get_ID: Integer;
    function Get_IndexName: WideString;
    function Get_IndexKind: Integer;
    function Get_FKRefDef_Obj_id: WideString;
    function Get_INDEXCOLUMNS: IXMLINDEXCOLUMNSType;
    procedure Set_ID(Value: Integer);
    procedure Set_IndexName(Value: WideString);
    procedure Set_IndexKind(Value: Integer);
    procedure Set_FKRefDef_Obj_id(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLINDEXCOLUMNSType }

  TXMLINDEXCOLUMNSType = class(TXMLNodeCollection, IXMLINDEXCOLUMNSType)
  protected
    { IXMLINDEXCOLUMNSType }
    function Get_INDEXCOLUMN(Index: Integer): IXMLINDEXCOLUMNType;
    function Add: IXMLINDEXCOLUMNType;
    function Insert(const Index: Integer): IXMLINDEXCOLUMNType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLINDEXCOLUMNType }

  TXMLINDEXCOLUMNType = class(TXMLNode, IXMLINDEXCOLUMNType)
  protected
    { IXMLINDEXCOLUMNType }
    function Get_IdColumn: Integer;
    function Get_LengthParam: Integer;
    procedure Set_IdColumn(Value: Integer);
    procedure Set_LengthParam(Value: Integer);
  end;

{ TXMLRELATIONSType }

  TXMLRELATIONSType = class(TXMLNodeCollection, IXMLRELATIONSType)
  protected
    { IXMLRELATIONSType }
    function Get_RELATION(Index: Integer): IXMLRELATIONType;
    function Add: IXMLRELATIONType;
    function Insert(const Index: Integer): IXMLRELATIONType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRELATIONType }

  TXMLRELATIONType = class(TXMLNode, IXMLRELATIONType)
  protected
    { IXMLRELATIONType }
    function Get_ID: Integer;
    function Get_RelationName: WideString;
    function Get_Kind: Integer;
    function Get_SrcTable: Integer;
    function Get_DestTable: Integer;
    function Get_FKFields: WideString;
    function Get_FKFieldsComments: WideString;
    function Get_RelDirection: Integer;
    function Get_MidOffset: WideString;
    function Get_OptionalStart: Integer;
    function Get_OptionalEnd: Integer;
    function Get_CaptionOffsetX: Integer;
    function Get_CaptionOffsetY: Integer;
    function Get_StartIntervalOffsetX: Integer;
    function Get_StartIntervalOffsetY: Integer;
    function Get_EndIntervalOffsetX: Integer;
    function Get_EndIntervalOffsetY: Integer;
    function Get_CreateRefDef: Integer;
    function Get_Invisible: Integer;
    function Get_RefDef: WideString;
    function Get_Comments: WideString;
    function Get_FKRefDefIndex_Obj_id: WideString;
    function Get_Splitted: Integer;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_RelationName(Value: WideString);
    procedure Set_Kind(Value: Integer);
    procedure Set_SrcTable(Value: Integer);
    procedure Set_DestTable(Value: Integer);
    procedure Set_FKFields(Value: WideString);
    procedure Set_FKFieldsComments(Value: WideString);
    procedure Set_RelDirection(Value: Integer);
    procedure Set_MidOffset(Value: WideString);
    procedure Set_OptionalStart(Value: Integer);
    procedure Set_OptionalEnd(Value: Integer);
    procedure Set_CaptionOffsetX(Value: Integer);
    procedure Set_CaptionOffsetY(Value: Integer);
    procedure Set_StartIntervalOffsetX(Value: Integer);
    procedure Set_StartIntervalOffsetY(Value: Integer);
    procedure Set_EndIntervalOffsetX(Value: Integer);
    procedure Set_EndIntervalOffsetY(Value: Integer);
    procedure Set_CreateRefDef(Value: Integer);
    procedure Set_Invisible(Value: Integer);
    procedure Set_RefDef(Value: WideString);
    procedure Set_Comments(Value: WideString);
    procedure Set_FKRefDefIndex_Obj_id(Value: WideString);
    procedure Set_Splitted(Value: Integer);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
  end;

{ TXMLNOTESType }

  TXMLNOTESType = class(TXMLNodeCollection, IXMLNOTESType)
  protected
    { IXMLNOTESType }
    function Get_NOTE(Index: Integer): IXMLNOTEType;
    function Add: IXMLNOTEType;
    function Insert(const Index: Integer): IXMLNOTEType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNOTEType }

  TXMLNOTEType = class(TXMLNode, IXMLNOTEType)
  protected
    { IXMLNOTEType }
    function Get_ID: Integer;
    function Get_NoteName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_NoteText: WideString;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_NoteName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_NoteText(Value: WideString);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
  end;

{ TXMLIMAGESType }

  TXMLIMAGESType = class(TXMLNodeCollection, IXMLIMAGESType)
  protected
    { IXMLIMAGESType }
    function Get_IMAGE(Index: Integer): IXMLIMAGEType;
    function Add: IXMLIMAGEType;
    function Insert(const Index: Integer): IXMLIMAGEType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLIMAGEType }

  TXMLIMAGEType = class(TXMLNode, IXMLIMAGEType)
  protected
    { IXMLIMAGEType }
    function Get_ID: Integer;
    function Get_ImageName: WideString;
    function Get_XPos: Integer;
    function Get_YPos: Integer;
    function Get_Width: Integer;
    function Get_Height: Integer;
    function Get_StrechImg: Integer;
    function Get_ImgWidth: Integer;
    function Get_ImgHeight: Integer;
    function Get_ImgFormat: WideString;
    function Get_ImgData: WideString;
    function Get_IsLinkedObject: Integer;
    function Get_IDLinkedModel: WideString;
    function Get_Obj_id_Linked: WideString;
    procedure Set_ID(Value: Integer);
    procedure Set_ImageName(Value: WideString);
    procedure Set_XPos(Value: Integer);
    procedure Set_YPos(Value: Integer);
    procedure Set_Width(Value: Integer);
    procedure Set_Height(Value: Integer);
    procedure Set_StrechImg(Value: Integer);
    procedure Set_ImgWidth(Value: Integer);
    procedure Set_ImgHeight(Value: Integer);
    procedure Set_ImgFormat(Value: WideString);
    procedure Set_ImgData(Value: WideString);
    procedure Set_IsLinkedObject(Value: Integer);
    procedure Set_IDLinkedModel(Value: WideString);
    procedure Set_Obj_id_Linked(Value: WideString);
  end;

{ TXMLPLUGINDATAType }

  TXMLPLUGINDATAType = class(TXMLNode, IXMLPLUGINDATAType)
  protected
    { IXMLPLUGINDATAType }
    function Get_PLUGINDATARECORDS: IXMLPLUGINDATARECORDSType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPLUGINDATARECORDSType }

  TXMLPLUGINDATARECORDSType = class(TXMLNodeCollection, IXMLPLUGINDATARECORDSType)
  protected
    { IXMLPLUGINDATARECORDSType }
    function Get_PLUGINDATARECORD(Index: Integer): IXMLPLUGINDATARECORDType;
    function Add: IXMLPLUGINDATARECORDType;
    function Insert(const Index: Integer): IXMLPLUGINDATARECORDType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPLUGINDATARECORDType }

  TXMLPLUGINDATARECORDType = class(TXMLNode, IXMLPLUGINDATARECORDType)
  protected
    { IXMLPLUGINDATARECORDType }
    function Get_PluginName: WideString;
    function Get_Obj_id: WideString;
    function Get_DataValue: WideString;
    function Get_PLUGINDATAPARAMS: IXMLPLUGINDATAPARAMSType;
    procedure Set_PluginName(Value: WideString);
    procedure Set_Obj_id(Value: WideString);
    procedure Set_DataValue(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPLUGINDATAPARAMSType }

  TXMLPLUGINDATAPARAMSType = class(TXMLNodeCollection, IXMLPLUGINDATAPARAMSType)
  protected
    { IXMLPLUGINDATAPARAMSType }
    function Get_PLUGINDATAPARAM(Index: Integer): IXMLPLUGINDATAPARAMType;
    function Add: IXMLPLUGINDATAPARAMType;
    function Insert(const Index: Integer): IXMLPLUGINDATAPARAMType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPLUGINDATAPARAMType }

  TXMLPLUGINDATAPARAMType = class(TXMLNode, IXMLPLUGINDATAPARAMType)
  protected
    { IXMLPLUGINDATAPARAMType }
    function Get_Value: WideString;
    procedure Set_Value(Value: WideString);
  end;

{ TXMLQUERYDATAType }

  TXMLQUERYDATAType = class(TXMLNode, IXMLQUERYDATAType)
  protected
    { IXMLQUERYDATAType }
    function Get_QUERYRECORDS: IXMLQUERYRECORDSType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLQUERYRECORDSType }

  TXMLQUERYRECORDSType = class(TXMLNodeCollection, IXMLQUERYRECORDSType)
  protected
    { IXMLQUERYRECORDSType }
    function Get_QUERYRECORD(Index: Integer): IXMLQUERYRECORDType;
    function Add: IXMLQUERYRECORDType;
    function Insert(const Index: Integer): IXMLQUERYRECORDType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLQUERYRECORDType }

  TXMLQUERYRECORDType = class(TXMLNode, IXMLQUERYRECORDType)
  protected
    { IXMLQUERYRECORDType }
    function Get_SQLCmdType: Integer;
    function Get_StoredPosition: WideString;
    function Get_SQLText: WideString;
    procedure Set_SQLCmdType(Value: Integer);
    procedure Set_StoredPosition(Value: WideString);
    procedure Set_SQLText(Value: WideString);
  end;

{ TXMLLINKEDMODELSType }

  TXMLLINKEDMODELSType = class(TXMLNodeCollection, IXMLLINKEDMODELSType)
  protected
    { IXMLLINKEDMODELSType }
    function Get_LINKEDMODEL(Index: Integer): IXMLLINKEDMODELType;
    function Add: IXMLLINKEDMODELType;
    function Insert(const Index: Integer): IXMLLINKEDMODELType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLLINKEDMODELType }

  TXMLLINKEDMODELType = class(TXMLNode, IXMLLINKEDMODELType)
  protected
    { IXMLLINKEDMODELType }
    function Get_IDLinkedModel: Integer;
    function Get_ModelName: WideString;
    function Get_IDModel: Integer;
    function Get_IsStoredInDB: Integer;
    function Get_ModelFilename: WideString;
    function Get_DriverName: WideString;
    function Get_DBConnName: WideString;
    function Get_HostCaption: WideString;
    function Get_HostName: WideString;
    function Get_Database: WideString;
    function Get_User: WideString;
    procedure Set_IDLinkedModel(Value: Integer);
    procedure Set_ModelName(Value: WideString);
    procedure Set_IDModel(Value: Integer);
    procedure Set_IsStoredInDB(Value: Integer);
    procedure Set_ModelFilename(Value: WideString);
    procedure Set_DriverName(Value: WideString);
    procedure Set_DBConnName(Value: WideString);
    procedure Set_HostCaption(Value: WideString);
    procedure Set_HostName(Value: WideString);
    procedure Set_Database(Value: WideString);
    procedure Set_User(Value: WideString);
  end;

{ Global Functions }

function GetDBMODEL(Doc: IXMLDocument): IXMLDBMODELType;
function LoadDBMODEL(const FileName: WideString): IXMLDBMODELType;
function NewDBMODEL: IXMLDBMODELType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetDBMODEL(Doc: IXMLDocument): IXMLDBMODELType;
begin
  Result := Doc.GetDocBinding('DBMODEL', TXMLDBMODELType, TargetNamespace) as IXMLDBMODELType;
end;

function LoadDBMODEL(const FileName: WideString): IXMLDBMODELType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('DBMODEL', TXMLDBMODELType, TargetNamespace) as IXMLDBMODELType;
end;

function NewDBMODEL: IXMLDBMODELType;
begin
  Result := NewXMLDocument.GetDocBinding('DBMODEL', TXMLDBMODELType, TargetNamespace) as IXMLDBMODELType;
end;

{ TXMLDBMODELType }

procedure TXMLDBMODELType.AfterConstruction;
begin
  RegisterChildNode('SETTINGS', TXMLSETTINGSType);
  RegisterChildNode('METADATA', TXMLMETADATAType);
  RegisterChildNode('PLUGINDATA', TXMLPLUGINDATAType);
  RegisterChildNode('QUERYDATA', TXMLQUERYDATAType);
  RegisterChildNode('LINKEDMODELS', TXMLLINKEDMODELSType);
  inherited;
end;

function TXMLDBMODELType.Get_Version: WideString;
begin
  Result := AttributeNodes['Version'].Text;
end;

procedure TXMLDBMODELType.Set_Version(Value: WideString);
begin
  SetAttribute('Version', Value);
end;

function TXMLDBMODELType.Get_SETTINGS: IXMLSETTINGSType;
begin
  Result := ChildNodes['SETTINGS'] as IXMLSETTINGSType;
end;

function TXMLDBMODELType.Get_METADATA: IXMLMETADATAType;
begin
  Result := ChildNodes['METADATA'] as IXMLMETADATAType;
end;

function TXMLDBMODELType.Get_PLUGINDATA: IXMLPLUGINDATAType;
begin
  Result := ChildNodes['PLUGINDATA'] as IXMLPLUGINDATAType;
end;

function TXMLDBMODELType.Get_QUERYDATA: IXMLQUERYDATAType;
begin
  Result := ChildNodes['QUERYDATA'] as IXMLQUERYDATAType;
end;

function TXMLDBMODELType.Get_LINKEDMODELS: IXMLLINKEDMODELSType;
begin
  Result := ChildNodes['LINKEDMODELS'] as IXMLLINKEDMODELSType;
end;

{ TXMLSETTINGSType }

procedure TXMLSETTINGSType.AfterConstruction;
begin
  RegisterChildNode('GLOBALSETTINGS', TXMLGLOBALSETTINGSType);
  RegisterChildNode('DATATYPEGROUPS', TXMLDATATYPEGROUPSType);
  RegisterChildNode('DATATYPES', TXMLDATATYPESType);
  RegisterChildNode('COMMON_DATATYPES', TXMLCOMMON_DATATYPESType);
  RegisterChildNode('TABLEPREFIXES', TXMLTABLEPREFIXESType);
  RegisterChildNode('REGIONCOLORS', TXMLREGIONCOLORSType);
  RegisterChildNode('POSITIONMARKERS', TXMLPOSITIONMARKERSType);
  inherited;
end;

function TXMLSETTINGSType.Get_GLOBALSETTINGS: IXMLGLOBALSETTINGSType;
begin
  Result := ChildNodes['GLOBALSETTINGS'] as IXMLGLOBALSETTINGSType;
end;

function TXMLSETTINGSType.Get_DATATYPEGROUPS: IXMLDATATYPEGROUPSType;
begin
  Result := ChildNodes['DATATYPEGROUPS'] as IXMLDATATYPEGROUPSType;
end;

function TXMLSETTINGSType.Get_DATATYPES: IXMLDATATYPESType;
begin
  Result := ChildNodes['DATATYPES'] as IXMLDATATYPESType;
end;

function TXMLSETTINGSType.Get_COMMON_DATATYPES: IXMLCOMMON_DATATYPESType;
begin
  Result := ChildNodes['COMMON_DATATYPES'] as IXMLCOMMON_DATATYPESType;
end;

function TXMLSETTINGSType.Get_TABLEPREFIXES: IXMLTABLEPREFIXESType;
begin
  Result := ChildNodes['TABLEPREFIXES'] as IXMLTABLEPREFIXESType;
end;

function TXMLSETTINGSType.Get_REGIONCOLORS: IXMLREGIONCOLORSType;
begin
  Result := ChildNodes['REGIONCOLORS'] as IXMLREGIONCOLORSType;
end;

function TXMLSETTINGSType.Get_POSITIONMARKERS: IXMLPOSITIONMARKERSType;
begin
  Result := ChildNodes['POSITIONMARKERS'] as IXMLPOSITIONMARKERSType;
end;

{ TXMLGLOBALSETTINGSType }

function TXMLGLOBALSETTINGSType.Get_ModelName: WideString;
begin
  Result := AttributeNodes['ModelName'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_ModelName(Value: WideString);
begin
  SetAttribute('ModelName', Value);
end;

function TXMLGLOBALSETTINGSType.Get_IDModel: Integer;
begin
  Result := AttributeNodes['IDModel'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_IDModel(Value: Integer);
begin
  SetAttribute('IDModel', Value);
end;

function TXMLGLOBALSETTINGSType.Get_IDVersion: Integer;
begin
  Result := AttributeNodes['IDVersion'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_IDVersion(Value: Integer);
begin
  SetAttribute('IDVersion', Value);
end;

function TXMLGLOBALSETTINGSType.Get_VersionStr: WideString;
begin
  Result := AttributeNodes['VersionStr'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_VersionStr(Value: WideString);
begin
  SetAttribute('VersionStr', Value);
end;

function TXMLGLOBALSETTINGSType.Get_Comments: WideString;
begin
  Result := AttributeNodes['Comments'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_Comments(Value: WideString);
begin
  SetAttribute('Comments', Value);
end;

function TXMLGLOBALSETTINGSType.Get_UseVersionHistroy: Integer;
begin
  Result := AttributeNodes['UseVersionHistroy'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_UseVersionHistroy(Value: Integer);
begin
  SetAttribute('UseVersionHistroy', Value);
end;

function TXMLGLOBALSETTINGSType.Get_AutoIncVersion: Integer;
begin
  Result := AttributeNodes['AutoIncVersion'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_AutoIncVersion(Value: Integer);
begin
  SetAttribute('AutoIncVersion', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DatabaseType: WideString;
begin
  Result := AttributeNodes['DatabaseType'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_DatabaseType(Value: WideString);
begin
  SetAttribute('DatabaseType', Value);
end;

function TXMLGLOBALSETTINGSType.Get_ZoomFac: WideString;
begin
  Result := AttributeNodes['ZoomFac'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_ZoomFac(Value: WideString);
begin
  SetAttribute('ZoomFac', Value);
end;

function TXMLGLOBALSETTINGSType.Get_XPos: Integer;
begin
  Result := AttributeNodes['XPos'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_XPos(Value: Integer);
begin
  SetAttribute('XPos', Value);
end;

function TXMLGLOBALSETTINGSType.Get_YPos: Integer;
begin
  Result := AttributeNodes['YPos'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_YPos(Value: Integer);
begin
  SetAttribute('YPos', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DefaultDataType: Integer;
begin
  Result := AttributeNodes['DefaultDataType'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_DefaultDataType(Value: Integer);
begin
  SetAttribute('DefaultDataType', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DefaultTablePrefix: Integer;
begin
  Result := AttributeNodes['DefaultTablePrefix'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_DefaultTablePrefix(Value: Integer);
begin
  SetAttribute('DefaultTablePrefix', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DefSaveDBConn: WideString;
begin
  Result := AttributeNodes['DefSaveDBConn'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_DefSaveDBConn(Value: WideString);
begin
  SetAttribute('DefSaveDBConn', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DefSyncDBConn: WideString;
begin
  Result := AttributeNodes['DefSyncDBConn'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_DefSyncDBConn(Value: WideString);
begin
  SetAttribute('DefSyncDBConn', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DefQueryDBConn: WideString;
begin
  Result := AttributeNodes['DefQueryDBConn'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_DefQueryDBConn(Value: WideString);
begin
  SetAttribute('DefQueryDBConn', Value);
end;

function TXMLGLOBALSETTINGSType.Get_Printer: WideString;
begin
  Result := AttributeNodes['Printer'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_Printer(Value: WideString);
begin
  SetAttribute('Printer', Value);
end;

function TXMLGLOBALSETTINGSType.Get_HPageCount: WideString;
begin
  Result := AttributeNodes['HPageCount'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_HPageCount(Value: WideString);
begin
  SetAttribute('HPageCount', Value);
end;

function TXMLGLOBALSETTINGSType.Get_PageAspectRatio: WideString;
begin
  Result := AttributeNodes['PageAspectRatio'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_PageAspectRatio(Value: WideString);
begin
  SetAttribute('PageAspectRatio', Value);
end;

function TXMLGLOBALSETTINGSType.Get_PageOrientation: Integer;
begin
  Result := AttributeNodes['PageOrientation'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_PageOrientation(Value: Integer);
begin
  SetAttribute('PageOrientation', Value);
end;

function TXMLGLOBALSETTINGSType.Get_PageFormat: WideString;
begin
  Result := AttributeNodes['PageFormat'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_PageFormat(Value: WideString);
begin
  SetAttribute('PageFormat', Value);
end;

function TXMLGLOBALSETTINGSType.Get_SelectedPages: WideString;
begin
  Result := AttributeNodes['SelectedPages'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_SelectedPages(Value: WideString);
begin
  SetAttribute('SelectedPages', Value);
end;

function TXMLGLOBALSETTINGSType.Get_UsePositionGrid: Integer;
begin
  Result := AttributeNodes['UsePositionGrid'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_UsePositionGrid(Value: Integer);
begin
  SetAttribute('UsePositionGrid', Value);
end;

function TXMLGLOBALSETTINGSType.Get_PositionGridX: Integer;
begin
  Result := AttributeNodes['PositionGridX'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_PositionGridX(Value: Integer);
begin
  SetAttribute('PositionGridX', Value);
end;

function TXMLGLOBALSETTINGSType.Get_PositionGridY: Integer;
begin
  Result := AttributeNodes['PositionGridY'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_PositionGridY(Value: Integer);
begin
  SetAttribute('PositionGridY', Value);
end;

function TXMLGLOBALSETTINGSType.Get_TableNameInRefs: Integer;
begin
  Result := AttributeNodes['TableNameInRefs'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_TableNameInRefs(Value: Integer);
begin
  SetAttribute('TableNameInRefs', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DefaultTableType: Integer;
begin
  Result := AttributeNodes['DefaultTableType'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_DefaultTableType(Value: Integer);
begin
  SetAttribute('DefaultTableType', Value);
end;

function TXMLGLOBALSETTINGSType.Get_ActivateRefDefForNewRelations: Integer;
begin
  Result := AttributeNodes['ActivateRefDefForNewRelations'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_ActivateRefDefForNewRelations(Value: Integer);
begin
  SetAttribute('ActivateRefDefForNewRelations', Value);
end;

function TXMLGLOBALSETTINGSType.Get_FKPrefix: WideString;
begin
  Result := AttributeNodes['FKPrefix'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_FKPrefix(Value: WideString);
begin
  SetAttribute('FKPrefix', Value);
end;

function TXMLGLOBALSETTINGSType.Get_FKPostfix: WideString;
begin
  Result := AttributeNodes['FKPostfix'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_FKPostfix(Value: WideString);
begin
  SetAttribute('FKPostfix', Value);
end;

function TXMLGLOBALSETTINGSType.Get_CreateFKRefDefIndex: Integer;
begin
  Result := AttributeNodes['CreateFKRefDefIndex'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_CreateFKRefDefIndex(Value: Integer);
begin
  SetAttribute('CreateFKRefDefIndex', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DBQuoteCharacter: WideString;
begin
  Result := AttributeNodes['DBQuoteCharacter'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_DBQuoteCharacter(Value: WideString);
begin
  SetAttribute('DBQuoteCharacter', Value);
end;

function TXMLGLOBALSETTINGSType.Get_CreateSQLforLinkedObjects: Integer;
begin
  Result := AttributeNodes['CreateSQLforLinkedObjects'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_CreateSQLforLinkedObjects(Value: Integer);
begin
  SetAttribute('CreateSQLforLinkedObjects', Value);
end;

function TXMLGLOBALSETTINGSType.Get_DefModelFont: WideString;
begin
  Result := AttributeNodes['DefModelFont'].Text;
end;

procedure TXMLGLOBALSETTINGSType.Set_DefModelFont(Value: WideString);
begin
  SetAttribute('DefModelFont', Value);
end;

function TXMLGLOBALSETTINGSType.Get_CanvasWidth: Integer;
begin
  Result := AttributeNodes['CanvasWidth'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_CanvasWidth(Value: Integer);
begin
  SetAttribute('CanvasWidth', Value);
end;

function TXMLGLOBALSETTINGSType.Get_CanvasHeight: Integer;
begin
  Result := AttributeNodes['CanvasHeight'].NodeValue;
end;

procedure TXMLGLOBALSETTINGSType.Set_CanvasHeight(Value: Integer);
begin
  SetAttribute('CanvasHeight', Value);
end;


{ TXMLDATATYPEGROUPSType }

procedure TXMLDATATYPEGROUPSType.AfterConstruction;
begin
  RegisterChildNode('DATATYPEGROUP', TXMLDATATYPEGROUPType);
  ItemTag := 'DATATYPEGROUP';
  ItemInterface := IXMLDATATYPEGROUPType;
  inherited;
end;

function TXMLDATATYPEGROUPSType.Get_DATATYPEGROUP(Index: Integer): IXMLDATATYPEGROUPType;
begin
  Result := List[Index] as IXMLDATATYPEGROUPType;
end;

function TXMLDATATYPEGROUPSType.Add: IXMLDATATYPEGROUPType;
begin
  Result := AddItem(-1) as IXMLDATATYPEGROUPType;
end;

function TXMLDATATYPEGROUPSType.Insert(const Index: Integer): IXMLDATATYPEGROUPType;
begin
  Result := AddItem(Index) as IXMLDATATYPEGROUPType;
end;

{ TXMLDATATYPEGROUPType }

function TXMLDATATYPEGROUPType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLDATATYPEGROUPType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLDATATYPEGROUPType.Get_Icon: Integer;
begin
  Result := AttributeNodes['Icon'].NodeValue;
end;

procedure TXMLDATATYPEGROUPType.Set_Icon(Value: Integer);
begin
  SetAttribute('Icon', Value);
end;

{ TXMLDATATYPESType }

procedure TXMLDATATYPESType.AfterConstruction;
begin
  RegisterChildNode('DATATYPE', TXMLDATATYPEType);
  ItemTag := 'DATATYPE';
  ItemInterface := IXMLDATATYPEType;
  inherited;
end;

function TXMLDATATYPESType.Get_DATATYPE(Index: Integer): IXMLDATATYPEType;
begin
  Result := List[Index] as IXMLDATATYPEType;
end;

function TXMLDATATYPESType.Add: IXMLDATATYPEType;
begin
  Result := AddItem(-1) as IXMLDATATYPEType;
end;

function TXMLDATATYPESType.Insert(const Index: Integer): IXMLDATATYPEType;
begin
  Result := AddItem(Index) as IXMLDATATYPEType;
end;

{ TXMLDATATYPEType }

procedure TXMLDATATYPEType.AfterConstruction;
begin
  RegisterChildNode('PARAMS', TXMLPARAMSType);
  RegisterChildNode('OPTIONS', TXMLOPTIONSType);
  inherited;
end;

function TXMLDATATYPEType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLDATATYPEType.Get_IDGroup: Integer;
begin
  Result := AttributeNodes['IDGroup'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_IDGroup(Value: Integer);
begin
  SetAttribute('IDGroup', Value);
end;

function TXMLDATATYPEType.Get_TypeName: WideString;
begin
  Result := AttributeNodes['TypeName'].Text;
end;

procedure TXMLDATATYPEType.Set_TypeName(Value: WideString);
begin
  SetAttribute('TypeName', Value);
end;

function TXMLDATATYPEType.Get_Description: WideString;
begin
  Result := AttributeNodes['Description'].Text;
end;

procedure TXMLDATATYPEType.Set_Description(Value: WideString);
begin
  SetAttribute('Description', Value);
end;

function TXMLDATATYPEType.Get_ParamCount: Integer;
begin
  Result := AttributeNodes['ParamCount'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_ParamCount(Value: Integer);
begin
  SetAttribute('ParamCount', Value);
end;

function TXMLDATATYPEType.Get_OptionCount: Integer;
begin
  Result := AttributeNodes['OptionCount'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_OptionCount(Value: Integer);
begin
  SetAttribute('OptionCount', Value);
end;

function TXMLDATATYPEType.Get_ParamRequired: Integer;
begin
  Result := AttributeNodes['ParamRequired'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_ParamRequired(Value: Integer);
begin
  SetAttribute('ParamRequired', Value);
end;

function TXMLDATATYPEType.Get_EditParamsAsString: Integer;
begin
  Result := AttributeNodes['EditParamsAsString'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_EditParamsAsString(Value: Integer);
begin
  SetAttribute('EditParamsAsString', Value);
end;

function TXMLDATATYPEType.Get_SynonymGroup: Integer;
begin
  Result := AttributeNodes['SynonymGroup'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_SynonymGroup(Value: Integer);
begin
  SetAttribute('SynonymGroup', Value);
end;

function TXMLDATATYPEType.Get_PhysicalMapping: Integer;
begin
  Result := AttributeNodes['PhysicalMapping'].NodeValue;
end;

procedure TXMLDATATYPEType.Set_PhysicalMapping(Value: Integer);
begin
  SetAttribute('PhysicalMapping', Value);
end;

function TXMLDATATYPEType.Get_PhysicalTypeName: WideString;
begin
  Result := AttributeNodes['PhysicalTypeName'].Text;
end;

procedure TXMLDATATYPEType.Set_PhysicalTypeName(Value: WideString);
begin
  SetAttribute('PhysicalTypeName', Value);
end;

function TXMLDATATYPEType.Get_PARAMS: IXMLPARAMSType;
begin
  Result := ChildNodes['PARAMS'] as IXMLPARAMSType;
end;

function TXMLDATATYPEType.Get_OPTIONS: IXMLOPTIONSType;
begin
  Result := ChildNodes['OPTIONS'] as IXMLOPTIONSType;
end;

{ TXMLPARAMSType }

procedure TXMLPARAMSType.AfterConstruction;
begin
  RegisterChildNode('PARAM', TXMLPARAMType);
  ItemTag := 'PARAM';
  ItemInterface := IXMLPARAMType;
  inherited;
end;

function TXMLPARAMSType.Get_PARAM(Index: Integer): IXMLPARAMType;
begin
  Result := List[Index] as IXMLPARAMType;
end;

function TXMLPARAMSType.Add: IXMLPARAMType;
begin
  Result := AddItem(-1) as IXMLPARAMType;
end;

function TXMLPARAMSType.Insert(const Index: Integer): IXMLPARAMType;
begin
  Result := AddItem(Index) as IXMLPARAMType;
end;

{ TXMLPARAMType }

function TXMLPARAMType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLPARAMType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

{ TXMLOPTIONSType }

procedure TXMLOPTIONSType.AfterConstruction;
begin
  RegisterChildNode('OPTION', TXMLOPTIONType);
  ItemTag := 'OPTION';
  ItemInterface := IXMLOPTIONType;
  inherited;
end;

function TXMLOPTIONSType.Get_OPTION(Index: Integer): IXMLOPTIONType;
begin
  Result := List[Index] as IXMLOPTIONType;
end;

function TXMLOPTIONSType.Add: IXMLOPTIONType;
begin
  Result := AddItem(-1) as IXMLOPTIONType;
end;

function TXMLOPTIONSType.Insert(const Index: Integer): IXMLOPTIONType;
begin
  Result := AddItem(Index) as IXMLOPTIONType;
end;

{ TXMLOPTIONType }

function TXMLOPTIONType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLOPTIONType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLOPTIONType.Get_Default: Integer;
begin
  Result := AttributeNodes['Default'].NodeValue;
end;

procedure TXMLOPTIONType.Set_Default(Value: Integer);
begin
  SetAttribute('Default', Value);
end;

{ TXMLCOMMON_DATATYPESType }

procedure TXMLCOMMON_DATATYPESType.AfterConstruction;
begin
  RegisterChildNode('COMMON_DATATYPE', TXMLCOMMON_DATATYPEType);
  ItemTag := 'COMMON_DATATYPE';
  ItemInterface := IXMLCOMMON_DATATYPEType;
  inherited;
end;

function TXMLCOMMON_DATATYPESType.Get_COMMON_DATATYPE(Index: Integer): IXMLCOMMON_DATATYPEType;
begin
  Result := List[Index] as IXMLCOMMON_DATATYPEType;
end;

function TXMLCOMMON_DATATYPESType.Add: IXMLCOMMON_DATATYPEType;
begin
  Result := AddItem(-1) as IXMLCOMMON_DATATYPEType;
end;

function TXMLCOMMON_DATATYPESType.Insert(const Index: Integer): IXMLCOMMON_DATATYPEType;
begin
  Result := AddItem(Index) as IXMLCOMMON_DATATYPEType;
end;

{ TXMLCOMMON_DATATYPEType }

function TXMLCOMMON_DATATYPEType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLCOMMON_DATATYPEType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

{ TXMLTABLEPREFIXESType }

procedure TXMLTABLEPREFIXESType.AfterConstruction;
begin
  RegisterChildNode('TABLEPREFIX', TXMLTABLEPREFIXType);
  ItemTag := 'TABLEPREFIX';
  ItemInterface := IXMLTABLEPREFIXType;
  inherited;
end;

function TXMLTABLEPREFIXESType.Get_TABLEPREFIX(Index: Integer): IXMLTABLEPREFIXType;
begin
  Result := List[Index] as IXMLTABLEPREFIXType;
end;

function TXMLTABLEPREFIXESType.Add: IXMLTABLEPREFIXType;
begin
  Result := AddItem(-1) as IXMLTABLEPREFIXType;
end;

function TXMLTABLEPREFIXESType.Insert(const Index: Integer): IXMLTABLEPREFIXType;
begin
  Result := AddItem(Index) as IXMLTABLEPREFIXType;
end;

{ TXMLTABLEPREFIXType }

function TXMLTABLEPREFIXType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLTABLEPREFIXType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

{ TXMLREGIONCOLORSType }

procedure TXMLREGIONCOLORSType.AfterConstruction;
begin
  RegisterChildNode('REGIONCOLOR', TXMLREGIONCOLORType);
  ItemTag := 'REGIONCOLOR';
  ItemInterface := IXMLREGIONCOLORType;
  inherited;
end;

function TXMLREGIONCOLORSType.Get_REGIONCOLOR(Index: Integer): IXMLREGIONCOLORType;
begin
  Result := List[Index] as IXMLREGIONCOLORType;
end;

function TXMLREGIONCOLORSType.Add: IXMLREGIONCOLORType;
begin
  Result := AddItem(-1) as IXMLREGIONCOLORType;
end;

function TXMLREGIONCOLORSType.Insert(const Index: Integer): IXMLREGIONCOLORType;
begin
  Result := AddItem(Index) as IXMLREGIONCOLORType;
end;

{ TXMLREGIONCOLORType }

function TXMLREGIONCOLORType.Get_Color: WideString;
begin
  Result := AttributeNodes['Color'].Text;
end;

procedure TXMLREGIONCOLORType.Set_Color(Value: WideString);
begin
  SetAttribute('Color', Value);
end;

{ TXMLPOSITIONMARKERSType }

procedure TXMLPOSITIONMARKERSType.AfterConstruction;
begin
  RegisterChildNode('POSITIONMARKER', TXMLPOSITIONMARKERType);
  ItemTag := 'POSITIONMARKER';
  ItemInterface := IXMLPOSITIONMARKERType;
  inherited;
end;

function TXMLPOSITIONMARKERSType.Get_POSITIONMARKER(Index: Integer): IXMLPOSITIONMARKERType;
begin
  Result := List[Index] as IXMLPOSITIONMARKERType;
end;

function TXMLPOSITIONMARKERSType.Add: IXMLPOSITIONMARKERType;
begin
  Result := AddItem(-1) as IXMLPOSITIONMARKERType;
end;

function TXMLPOSITIONMARKERSType.Insert(const Index: Integer): IXMLPOSITIONMARKERType;
begin
  Result := AddItem(Index) as IXMLPOSITIONMARKERType;
end;

{ TXMLPOSITIONMARKERType }

function TXMLPOSITIONMARKERType.Get_ZoomFac: WideString;
begin
  Result := AttributeNodes['ZoomFac'].Text;
end;

procedure TXMLPOSITIONMARKERType.Set_ZoomFac(Value: WideString);
begin
  SetAttribute('ZoomFac', Value);
end;

function TXMLPOSITIONMARKERType.Get_X: Integer;
begin
  Result := AttributeNodes['X'].NodeValue;
end;

procedure TXMLPOSITIONMARKERType.Set_X(Value: Integer);
begin
  SetAttribute('X', Value);
end;

function TXMLPOSITIONMARKERType.Get_Y: Integer;
begin
  Result := AttributeNodes['Y'].NodeValue;
end;

procedure TXMLPOSITIONMARKERType.Set_Y(Value: Integer);
begin
  SetAttribute('Y', Value);
end;

{ TXMLMETADATAType }

procedure TXMLMETADATAType.AfterConstruction;
begin
  RegisterChildNode('REGIONS', TXMLREGIONSType);
  RegisterChildNode('TABLES', TXMLTABLESType);
  RegisterChildNode('RELATIONS', TXMLRELATIONSType);
  RegisterChildNode('NOTES', TXMLNOTESType);
  RegisterChildNode('IMAGES', TXMLIMAGESType);
  inherited;
end;

function TXMLMETADATAType.Get_REGIONS: IXMLREGIONSType;
begin
  Result := ChildNodes['REGIONS'] as IXMLREGIONSType;
end;

function TXMLMETADATAType.Get_TABLES: IXMLTABLESType;
begin
  Result := ChildNodes['TABLES'] as IXMLTABLESType;
end;

function TXMLMETADATAType.Get_RELATIONS: IXMLRELATIONSType;
begin
  Result := ChildNodes['RELATIONS'] as IXMLRELATIONSType;
end;

function TXMLMETADATAType.Get_NOTES: IXMLNOTESType;
begin
  Result := ChildNodes['NOTES'] as IXMLNOTESType;
end;

function TXMLMETADATAType.Get_IMAGES: IXMLIMAGESType;
begin
  Result := ChildNodes['IMAGES'] as IXMLIMAGESType;
end;

{ TXMLREGIONSType }

procedure TXMLREGIONSType.AfterConstruction;
begin
  RegisterChildNode('REGION', TXMLREGIONType);
  ItemTag := 'REGION';
  ItemInterface := IXMLREGIONType;
  inherited;
end;

function TXMLREGIONSType.Get_REGION(Index: Integer): IXMLREGIONType;
begin
  Result := List[Index] as IXMLREGIONType;
end;

function TXMLREGIONSType.Add: IXMLREGIONType;
begin
  Result := AddItem(-1) as IXMLREGIONType;
end;

function TXMLREGIONSType.Insert(const Index: Integer): IXMLREGIONType;
begin
  Result := AddItem(Index) as IXMLREGIONType;
end;

{ TXMLREGIONType }

function TXMLREGIONType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLREGIONType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLREGIONType.Get_RegionName: WideString;
begin
  Result := AttributeNodes['RegionName'].Text;
end;

procedure TXMLREGIONType.Set_RegionName(Value: WideString);
begin
  SetAttribute('RegionName', Value);
end;

function TXMLREGIONType.Get_XPos: Integer;
begin
  Result := AttributeNodes['XPos'].NodeValue;
end;

procedure TXMLREGIONType.Set_XPos(Value: Integer);
begin
  SetAttribute('XPos', Value);
end;

function TXMLREGIONType.Get_YPos: Integer;
begin
  Result := AttributeNodes['YPos'].NodeValue;
end;

procedure TXMLREGIONType.Set_YPos(Value: Integer);
begin
  SetAttribute('YPos', Value);
end;

function TXMLREGIONType.Get_Width: Integer;
begin
  Result := AttributeNodes['Width'].NodeValue;
end;

procedure TXMLREGIONType.Set_Width(Value: Integer);
begin
  SetAttribute('Width', Value);
end;

function TXMLREGIONType.Get_Height: Integer;
begin
  Result := AttributeNodes['Height'].NodeValue;
end;

procedure TXMLREGIONType.Set_Height(Value: Integer);
begin
  SetAttribute('Height', Value);
end;

function TXMLREGIONType.Get_RegionColor: Integer;
begin
  Result := AttributeNodes['RegionColor'].NodeValue;
end;

procedure TXMLREGIONType.Set_RegionColor(Value: Integer);
begin
  SetAttribute('RegionColor', Value);
end;

function TXMLREGIONType.Get_TablePrefix: Integer;
begin
  Result := AttributeNodes['TablePrefix'].NodeValue;
end;

procedure TXMLREGIONType.Set_TablePrefix(Value: Integer);
begin
  SetAttribute('TablePrefix', Value);
end;

function TXMLREGIONType.Get_TableType: Integer;
begin
  Result := AttributeNodes['TableType'].NodeValue;
end;

procedure TXMLREGIONType.Set_TableType(Value: Integer);
begin
  SetAttribute('TableType', Value);
end;

function TXMLREGIONType.Get_OverwriteTablePrefix: Integer;
begin
  Result := AttributeNodes['OverwriteTablePrefix'].NodeValue;
end;

procedure TXMLREGIONType.Set_OverwriteTablePrefix(Value: Integer);
begin
  SetAttribute('OverwriteTablePrefix', Value);
end;

function TXMLREGIONType.Get_OverwriteTableType: Integer;
begin
  Result := AttributeNodes['OverwriteTableType'].NodeValue;
end;

procedure TXMLREGIONType.Set_OverwriteTableType(Value: Integer);
begin
  SetAttribute('OverwriteTableType', Value);
end;

function TXMLREGIONType.Get_Comments: WideString;
begin
  Result := AttributeNodes['Comments'].Text;
end;

procedure TXMLREGIONType.Set_Comments(Value: WideString);
begin
  SetAttribute('Comments', Value);
end;

function TXMLREGIONType.Get_IsLinkedObject: Integer;
begin
  Result := AttributeNodes['IsLinkedObject'].NodeValue;
end;

procedure TXMLREGIONType.Set_IsLinkedObject(Value: Integer);
begin
  SetAttribute('IsLinkedObject', Value);
end;

function TXMLREGIONType.Get_IDLinkedModel: WideString;
begin
  Result := AttributeNodes['IDLinkedModel'].Text;
end;

procedure TXMLREGIONType.Set_IDLinkedModel(Value: WideString);
begin
  SetAttribute('IDLinkedModel', Value);
end;

function TXMLREGIONType.Get_Obj_id_Linked: WideString;
begin
  Result := AttributeNodes['Obj_id_Linked'].Text;
end;

procedure TXMLREGIONType.Set_Obj_id_Linked(Value: WideString);
begin
  SetAttribute('Obj_id_Linked', Value);
end;

{ TXMLTABLESType }

procedure TXMLTABLESType.AfterConstruction;
begin
  RegisterChildNode('TABLE', TXMLTABLEType);
  ItemTag := 'TABLE';
  ItemInterface := IXMLTABLEType;
  inherited;
end;

function TXMLTABLESType.Get_TABLE(Index: Integer): IXMLTABLEType;
begin
  Result := List[Index] as IXMLTABLEType;
end;

function TXMLTABLESType.Add: IXMLTABLEType;
begin
  Result := AddItem(-1) as IXMLTABLEType;
end;

function TXMLTABLESType.Insert(const Index: Integer): IXMLTABLEType;
begin
  Result := AddItem(Index) as IXMLTABLEType;
end;

{ TXMLTABLEType }

procedure TXMLTABLEType.AfterConstruction;
begin
  RegisterChildNode('COLUMNS', TXMLCOLUMNSType);
  RegisterChildNode('RELATIONS_START', TXMLRELATIONS_STARTType);
  RegisterChildNode('RELATIONS_END', TXMLRELATIONS_ENDType);
  RegisterChildNode('INDICES', TXMLINDICESType);
  inherited;
end;

function TXMLTABLEType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLTABLEType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLTABLEType.Get_Tablename: WideString;
begin
  Result := AttributeNodes['Tablename'].Text;
end;

procedure TXMLTABLEType.Set_Tablename(Value: WideString);
begin
  SetAttribute('Tablename', Value);
end;

function TXMLTABLEType.Get_PrevTableName: WideString;
begin
  Result := AttributeNodes['PrevTableName'].Text;
end;

procedure TXMLTABLEType.Set_PrevTableName(Value: WideString);
begin
  SetAttribute('PrevTableName', Value);
end;

function TXMLTABLEType.Get_XPos: Integer;
begin
  Result := AttributeNodes['XPos'].NodeValue;
end;

procedure TXMLTABLEType.Set_XPos(Value: Integer);
begin
  SetAttribute('XPos', Value);
end;

function TXMLTABLEType.Get_YPos: Integer;
begin
  Result := AttributeNodes['YPos'].NodeValue;
end;

procedure TXMLTABLEType.Set_YPos(Value: Integer);
begin
  SetAttribute('YPos', Value);
end;

function TXMLTABLEType.Get_TableType: Integer;
begin
  Result := AttributeNodes['TableType'].NodeValue;
end;

procedure TXMLTABLEType.Set_TableType(Value: Integer);
begin
  SetAttribute('TableType', Value);
end;

function TXMLTABLEType.Get_TablePrefix: Integer;
begin
  Result := AttributeNodes['TablePrefix'].NodeValue;
end;

procedure TXMLTABLEType.Set_TablePrefix(Value: Integer);
begin
  SetAttribute('TablePrefix', Value);
end;

function TXMLTABLEType.Get_NmTable: Integer;
begin
  Result := AttributeNodes['nmTable'].NodeValue;
end;

procedure TXMLTABLEType.Set_NmTable(Value: Integer);
begin
  SetAttribute('nmTable', Value);
end;

function TXMLTABLEType.Get_Temporary: Integer;
begin
  Result := AttributeNodes['Temporary'].NodeValue;
end;

procedure TXMLTABLEType.Set_Temporary(Value: Integer);
begin
  SetAttribute('Temporary', Value);
end;

function TXMLTABLEType.Get_UseStandardInserts: Integer;
begin
  Result := AttributeNodes['UseStandardInserts'].NodeValue;
end;

procedure TXMLTABLEType.Set_UseStandardInserts(Value: Integer);
begin
  SetAttribute('UseStandardInserts', Value);
end;

function TXMLTABLEType.Get_StandardInserts: WideString;
begin
  Result := AttributeNodes['StandardInserts'].Text;
end;

procedure TXMLTABLEType.Set_StandardInserts(Value: WideString);
begin
  SetAttribute('StandardInserts', Value);
end;

function TXMLTABLEType.Get_TableOptions: WideString;
begin
  Result := AttributeNodes['TableOptions'].Text;
end;

procedure TXMLTABLEType.Set_TableOptions(Value: WideString);
begin
  SetAttribute('TableOptions', Value);
end;

function TXMLTABLEType.Get_Comments: WideString;
begin
  Result := AttributeNodes['Comments'].Text;
end;

procedure TXMLTABLEType.Set_Comments(Value: WideString);
begin
  SetAttribute('Comments', Value);
end;

function TXMLTABLEType.Get_Collapsed: Integer;
begin
  Result := AttributeNodes['Collapsed'].NodeValue;
end;

procedure TXMLTABLEType.Set_Collapsed(Value: Integer);
begin
  SetAttribute('Collapsed', Value);
end;

function TXMLTABLEType.Get_IsLinkedObject: Integer;
begin
  Result := AttributeNodes['IsLinkedObject'].NodeValue;
end;

procedure TXMLTABLEType.Set_IsLinkedObject(Value: Integer);
begin
  SetAttribute('IsLinkedObject', Value);
end;

function TXMLTABLEType.Get_IDLinkedModel: WideString;
begin
  Result := AttributeNodes['IDLinkedModel'].Text;
end;

procedure TXMLTABLEType.Set_IDLinkedModel(Value: WideString);
begin
  SetAttribute('IDLinkedModel', Value);
end;

function TXMLTABLEType.Get_Obj_id_Linked: WideString;
begin
  Result := AttributeNodes['Obj_id_Linked'].Text;
end;

procedure TXMLTABLEType.Set_Obj_id_Linked(Value: WideString);
begin
  SetAttribute('Obj_id_Linked', Value);
end;

function TXMLTABLEType.Get_COLUMNS: IXMLCOLUMNSType;
begin
  Result := ChildNodes['COLUMNS'] as IXMLCOLUMNSType;
end;

function TXMLTABLEType.Get_RELATIONS_START: IXMLRELATIONS_STARTType;
begin
  Result := ChildNodes['RELATIONS_START'] as IXMLRELATIONS_STARTType;
end;

function TXMLTABLEType.Get_RELATIONS_END: IXMLRELATIONS_ENDType;
begin
  Result := ChildNodes['RELATIONS_END'] as IXMLRELATIONS_ENDType;
end;

function TXMLTABLEType.Get_INDICES: IXMLINDICESType;
begin
  Result := ChildNodes['INDICES'] as IXMLINDICESType;
end;

{ TXMLCOLUMNSType }

procedure TXMLCOLUMNSType.AfterConstruction;
begin
  RegisterChildNode('COLUMN', TXMLCOLUMNType);
  ItemTag := 'COLUMN';
  ItemInterface := IXMLCOLUMNType;
  inherited;
end;

function TXMLCOLUMNSType.Get_COLUMN(Index: Integer): IXMLCOLUMNType;
begin
  Result := List[Index] as IXMLCOLUMNType;
end;

function TXMLCOLUMNSType.Add: IXMLCOLUMNType;
begin
  Result := AddItem(-1) as IXMLCOLUMNType;
end;

function TXMLCOLUMNSType.Insert(const Index: Integer): IXMLCOLUMNType;
begin
  Result := AddItem(Index) as IXMLCOLUMNType;
end;

{ TXMLCOLUMNType }

procedure TXMLCOLUMNType.AfterConstruction;
begin
  RegisterChildNode('OPTIONSELECTED', TXMLOPTIONSELECTEDType);
  inherited;
end;

function TXMLCOLUMNType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLCOLUMNType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLCOLUMNType.Get_ColName: WideString;
begin
  Result := AttributeNodes['ColName'].Text;
end;

procedure TXMLCOLUMNType.Set_ColName(Value: WideString);
begin
  SetAttribute('ColName', Value);
end;

function TXMLCOLUMNType.Get_PrevColName: WideString;
begin
  Result := AttributeNodes['PrevColName'].Text;
end;

procedure TXMLCOLUMNType.Set_PrevColName(Value: WideString);
begin
  SetAttribute('PrevColName', Value);
end;

function TXMLCOLUMNType.Get_Pos: Integer;
begin
  Result := AttributeNodes['Pos'].NodeValue;
end;

procedure TXMLCOLUMNType.Set_Pos(Value: Integer);
begin
  SetAttribute('Pos', Value);
end;

function TXMLCOLUMNType.Get_IdDatatype: Integer;
begin
  Result := AttributeNodes['idDatatype'].NodeValue;
end;

procedure TXMLCOLUMNType.Set_IdDatatype(Value: Integer);
begin
  SetAttribute('idDatatype', Value);
end;

function TXMLCOLUMNType.Get_DatatypeParams: WideString;
begin
  Result := AttributeNodes['DatatypeParams'].Text;
end;

procedure TXMLCOLUMNType.Set_DatatypeParams(Value: WideString);
begin
  SetAttribute('DatatypeParams', Value);
end;

function TXMLCOLUMNType.Get_Width: WideString;
begin
  Result := AttributeNodes['Width'].Text;
end;

procedure TXMLCOLUMNType.Set_Width(Value: WideString);
begin
  SetAttribute('Width', Value);
end;

function TXMLCOLUMNType.Get_Prec: WideString;
begin
  Result := AttributeNodes['Prec'].Text;
end;

procedure TXMLCOLUMNType.Set_Prec(Value: WideString);
begin
  SetAttribute('Prec', Value);
end;

function TXMLCOLUMNType.Get_PrimaryKey: Integer;
begin
  Result := AttributeNodes['PrimaryKey'].NodeValue;
end;

procedure TXMLCOLUMNType.Set_PrimaryKey(Value: Integer);
begin
  SetAttribute('PrimaryKey', Value);
end;

function TXMLCOLUMNType.Get_NotNull: Integer;
begin
  Result := AttributeNodes['NotNull'].NodeValue;
end;

procedure TXMLCOLUMNType.Set_NotNull(Value: Integer);
begin
  SetAttribute('NotNull', Value);
end;

function TXMLCOLUMNType.Get_AutoInc: Integer;
begin
  Result := AttributeNodes['AutoInc'].NodeValue;
end;

procedure TXMLCOLUMNType.Set_AutoInc(Value: Integer);
begin
  SetAttribute('AutoInc', Value);
end;

function TXMLCOLUMNType.Get_IsForeignKey: Integer;
begin
  Result := AttributeNodes['IsForeignKey'].NodeValue;
end;

procedure TXMLCOLUMNType.Set_IsForeignKey(Value: Integer);
begin
  SetAttribute('IsForeignKey', Value);
end;

function TXMLCOLUMNType.Get_DefaultValue: WideString;
begin
  Result := AttributeNodes['DefaultValue'].Text;
end;

procedure TXMLCOLUMNType.Set_DefaultValue(Value: WideString);
begin
  SetAttribute('DefaultValue', Value);
end;

function TXMLCOLUMNType.Get_Comments: WideString;
begin
  Result := AttributeNodes['Comments'].Text;
end;

procedure TXMLCOLUMNType.Set_Comments(Value: WideString);
begin
  SetAttribute('Comments', Value);
end;

function TXMLCOLUMNType.Get_OPTIONSELECTED: IXMLOPTIONSELECTEDType;
begin
  Result := ChildNodes['OPTIONSELECTED'] as IXMLOPTIONSELECTEDType;
end;

{ TXMLOPTIONSELECTEDType }

procedure TXMLOPTIONSELECTEDType.AfterConstruction;
begin
  RegisterChildNode('OPTIONSELECT', TXMLOPTIONSELECTType);
  ItemTag := 'OPTIONSELECT';
  ItemInterface := IXMLOPTIONSELECTType;
  inherited;
end;

function TXMLOPTIONSELECTEDType.Get_OPTIONSELECT(Index: Integer): IXMLOPTIONSELECTType;
begin
  Result := List[Index] as IXMLOPTIONSELECTType;
end;

function TXMLOPTIONSELECTEDType.Add: IXMLOPTIONSELECTType;
begin
  Result := AddItem(-1) as IXMLOPTIONSELECTType;
end;

function TXMLOPTIONSELECTEDType.Insert(const Index: Integer): IXMLOPTIONSELECTType;
begin
  Result := AddItem(Index) as IXMLOPTIONSELECTType;
end;

{ TXMLOPTIONSELECTType }

function TXMLOPTIONSELECTType.Get_Value: Integer;
begin
  Result := AttributeNodes['Value'].NodeValue;
end;

procedure TXMLOPTIONSELECTType.Set_Value(Value: Integer);
begin
  SetAttribute('Value', Value);
end;

{ TXMLRELATIONS_STARTType }

procedure TXMLRELATIONS_STARTType.AfterConstruction;
begin
  RegisterChildNode('RELATION_START', TXMLRELATION_STARTType);
  ItemTag := 'RELATION_START';
  ItemInterface := IXMLRELATION_STARTType;
  inherited;
end;

function TXMLRELATIONS_STARTType.Get_RELATION_START(Index: Integer): IXMLRELATION_STARTType;
begin
  Result := List[Index] as IXMLRELATION_STARTType;
end;

function TXMLRELATIONS_STARTType.Add: IXMLRELATION_STARTType;
begin
  Result := AddItem(-1) as IXMLRELATION_STARTType;
end;

function TXMLRELATIONS_STARTType.Insert(const Index: Integer): IXMLRELATION_STARTType;
begin
  Result := AddItem(Index) as IXMLRELATION_STARTType;
end;

{ TXMLRELATION_STARTType }

function TXMLRELATION_STARTType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLRELATION_STARTType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

{ TXMLRELATIONS_ENDType }

procedure TXMLRELATIONS_ENDType.AfterConstruction;
begin
  RegisterChildNode('RELATION_END', TXMLRELATION_ENDType);
  ItemTag := 'RELATION_END';
  ItemInterface := IXMLRELATION_ENDType;
  inherited;
end;

function TXMLRELATIONS_ENDType.Get_RELATION_END(Index: Integer): IXMLRELATION_ENDType;
begin
  Result := List[Index] as IXMLRELATION_ENDType;
end;

function TXMLRELATIONS_ENDType.Add: IXMLRELATION_ENDType;
begin
  Result := AddItem(-1) as IXMLRELATION_ENDType;
end;

function TXMLRELATIONS_ENDType.Insert(const Index: Integer): IXMLRELATION_ENDType;
begin
  Result := AddItem(Index) as IXMLRELATION_ENDType;
end;

{ TXMLRELATION_ENDType }

function TXMLRELATION_ENDType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLRELATION_ENDType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

{ TXMLINDICESType }

procedure TXMLINDICESType.AfterConstruction;
begin
  RegisterChildNode('INDEX', TXMLINDEXType);
  ItemTag := 'INDEX';
  ItemInterface := IXMLINDEXType;
  inherited;
end;

function TXMLINDICESType.Get_INDEX(Index: Integer): IXMLINDEXType;
begin
  Result := List[Index] as IXMLINDEXType;
end;

function TXMLINDICESType.Add: IXMLINDEXType;
begin
  Result := AddItem(-1) as IXMLINDEXType;
end;

function TXMLINDICESType.Insert(const Index: Integer): IXMLINDEXType;
begin
  Result := AddItem(Index) as IXMLINDEXType;
end;

{ TXMLINDEXType }

procedure TXMLINDEXType.AfterConstruction;
begin
  RegisterChildNode('INDEXCOLUMNS', TXMLINDEXCOLUMNSType);
  inherited;
end;

function TXMLINDEXType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLINDEXType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLINDEXType.Get_IndexName: WideString;
begin
  Result := AttributeNodes['IndexName'].Text;
end;

procedure TXMLINDEXType.Set_IndexName(Value: WideString);
begin
  SetAttribute('IndexName', Value);
end;

function TXMLINDEXType.Get_IndexKind: Integer;
begin
  Result := AttributeNodes['IndexKind'].NodeValue;
end;

procedure TXMLINDEXType.Set_IndexKind(Value: Integer);
begin
  SetAttribute('IndexKind', Value);
end;

function TXMLINDEXType.Get_FKRefDef_Obj_id: WideString;
begin
  Result := AttributeNodes['FKRefDef_Obj_id'].Text;
end;

procedure TXMLINDEXType.Set_FKRefDef_Obj_id(Value: WideString);
begin
  SetAttribute('FKRefDef_Obj_id', Value);
end;

function TXMLINDEXType.Get_INDEXCOLUMNS: IXMLINDEXCOLUMNSType;
begin
  Result := ChildNodes['INDEXCOLUMNS'] as IXMLINDEXCOLUMNSType;
end;

{ TXMLINDEXCOLUMNSType }

procedure TXMLINDEXCOLUMNSType.AfterConstruction;
begin
  RegisterChildNode('INDEXCOLUMN', TXMLINDEXCOLUMNType);
  ItemTag := 'INDEXCOLUMN';
  ItemInterface := IXMLINDEXCOLUMNType;
  inherited;
end;

function TXMLINDEXCOLUMNSType.Get_INDEXCOLUMN(Index: Integer): IXMLINDEXCOLUMNType;
begin
  Result := List[Index] as IXMLINDEXCOLUMNType;
end;

function TXMLINDEXCOLUMNSType.Add: IXMLINDEXCOLUMNType;
begin
  Result := AddItem(-1) as IXMLINDEXCOLUMNType;
end;

function TXMLINDEXCOLUMNSType.Insert(const Index: Integer): IXMLINDEXCOLUMNType;
begin
  Result := AddItem(Index) as IXMLINDEXCOLUMNType;
end;

{ TXMLINDEXCOLUMNType }

function TXMLINDEXCOLUMNType.Get_IdColumn: Integer;
begin
  Result := AttributeNodes['idColumn'].NodeValue;
end;

procedure TXMLINDEXCOLUMNType.Set_IdColumn(Value: Integer);
begin
  SetAttribute('idColumn', Value);
end;

function TXMLINDEXCOLUMNType.Get_LengthParam: Integer;
begin
  Result := AttributeNodes['LengthParam'].NodeValue;
end;

procedure TXMLINDEXCOLUMNType.Set_LengthParam(Value: Integer);
begin
  SetAttribute('LengthParam', Value);
end;

{ TXMLRELATIONSType }

procedure TXMLRELATIONSType.AfterConstruction;
begin
  RegisterChildNode('RELATION', TXMLRELATIONType);
  ItemTag := 'RELATION';
  ItemInterface := IXMLRELATIONType;
  inherited;
end;

function TXMLRELATIONSType.Get_RELATION(Index: Integer): IXMLRELATIONType;
begin
  Result := List[Index] as IXMLRELATIONType;
end;

function TXMLRELATIONSType.Add: IXMLRELATIONType;
begin
  Result := AddItem(-1) as IXMLRELATIONType;
end;

function TXMLRELATIONSType.Insert(const Index: Integer): IXMLRELATIONType;
begin
  Result := AddItem(Index) as IXMLRELATIONType;
end;

{ TXMLRELATIONType }

function TXMLRELATIONType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLRELATIONType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLRELATIONType.Get_RelationName: WideString;
begin
  Result := AttributeNodes['RelationName'].Text;
end;

procedure TXMLRELATIONType.Set_RelationName(Value: WideString);
begin
  SetAttribute('RelationName', Value);
end;

function TXMLRELATIONType.Get_Kind: Integer;
begin
  Result := AttributeNodes['Kind'].NodeValue;
end;

procedure TXMLRELATIONType.Set_Kind(Value: Integer);
begin
  SetAttribute('Kind', Value);
end;

function TXMLRELATIONType.Get_SrcTable: Integer;
begin
  Result := AttributeNodes['SrcTable'].NodeValue;
end;

procedure TXMLRELATIONType.Set_SrcTable(Value: Integer);
begin
  SetAttribute('SrcTable', Value);
end;

function TXMLRELATIONType.Get_DestTable: Integer;
begin
  Result := AttributeNodes['DestTable'].NodeValue;
end;

procedure TXMLRELATIONType.Set_DestTable(Value: Integer);
begin
  SetAttribute('DestTable', Value);
end;

function TXMLRELATIONType.Get_FKFields: WideString;
begin
  Result := AttributeNodes['FKFields'].Text;
end;

procedure TXMLRELATIONType.Set_FKFields(Value: WideString);
begin
  SetAttribute('FKFields', Value);
end;

function TXMLRELATIONType.Get_FKFieldsComments: WideString;
begin
  Result := AttributeNodes['FKFieldsComments'].Text;
end;

procedure TXMLRELATIONType.Set_FKFieldsComments(Value: WideString);
begin
  SetAttribute('FKFieldsComments', Value);
end;

function TXMLRELATIONType.Get_RelDirection: Integer;
begin
  Result := AttributeNodes['relDirection'].NodeValue;
end;

procedure TXMLRELATIONType.Set_RelDirection(Value: Integer);
begin
  SetAttribute('relDirection', Value);
end;

function TXMLRELATIONType.Get_MidOffset: WideString;
begin
  Result := AttributeNodes['MidOffset'].Text;
end;

procedure TXMLRELATIONType.Set_MidOffset(Value: WideString);
begin
  SetAttribute('MidOffset', Value);
end;

function TXMLRELATIONType.Get_OptionalStart: Integer;
begin
  Result := AttributeNodes['OptionalStart'].NodeValue;
end;

procedure TXMLRELATIONType.Set_OptionalStart(Value: Integer);
begin
  SetAttribute('OptionalStart', Value);
end;

function TXMLRELATIONType.Get_OptionalEnd: Integer;
begin
  Result := AttributeNodes['OptionalEnd'].NodeValue;
end;

procedure TXMLRELATIONType.Set_OptionalEnd(Value: Integer);
begin
  SetAttribute('OptionalEnd', Value);
end;

function TXMLRELATIONType.Get_CaptionOffsetX: Integer;
begin
  Result := AttributeNodes['CaptionOffsetX'].NodeValue;
end;

procedure TXMLRELATIONType.Set_CaptionOffsetX(Value: Integer);
begin
  SetAttribute('CaptionOffsetX', Value);
end;

function TXMLRELATIONType.Get_CaptionOffsetY: Integer;
begin
  Result := AttributeNodes['CaptionOffsetY'].NodeValue;
end;

procedure TXMLRELATIONType.Set_CaptionOffsetY(Value: Integer);
begin
  SetAttribute('CaptionOffsetY', Value);
end;

function TXMLRELATIONType.Get_StartIntervalOffsetX: Integer;
begin
  Result := AttributeNodes['StartIntervalOffsetX'].NodeValue;
end;

procedure TXMLRELATIONType.Set_StartIntervalOffsetX(Value: Integer);
begin
  SetAttribute('StartIntervalOffsetX', Value);
end;

function TXMLRELATIONType.Get_StartIntervalOffsetY: Integer;
begin
  Result := AttributeNodes['StartIntervalOffsetY'].NodeValue;
end;

procedure TXMLRELATIONType.Set_StartIntervalOffsetY(Value: Integer);
begin
  SetAttribute('StartIntervalOffsetY', Value);
end;

function TXMLRELATIONType.Get_EndIntervalOffsetX: Integer;
begin
  Result := AttributeNodes['EndIntervalOffsetX'].NodeValue;
end;

procedure TXMLRELATIONType.Set_EndIntervalOffsetX(Value: Integer);
begin
  SetAttribute('EndIntervalOffsetX', Value);
end;

function TXMLRELATIONType.Get_EndIntervalOffsetY: Integer;
begin
  Result := AttributeNodes['EndIntervalOffsetY'].NodeValue;
end;

procedure TXMLRELATIONType.Set_EndIntervalOffsetY(Value: Integer);
begin
  SetAttribute('EndIntervalOffsetY', Value);
end;

function TXMLRELATIONType.Get_CreateRefDef: Integer;
begin
  Result := AttributeNodes['CreateRefDef'].NodeValue;
end;

procedure TXMLRELATIONType.Set_CreateRefDef(Value: Integer);
begin
  SetAttribute('CreateRefDef', Value);
end;

function TXMLRELATIONType.Get_Invisible: Integer;
begin
  Result := AttributeNodes['Invisible'].NodeValue;
end;

procedure TXMLRELATIONType.Set_Invisible(Value: Integer);
begin
  SetAttribute('Invisible', Value);
end;

function TXMLRELATIONType.Get_RefDef: WideString;
begin
  Result := AttributeNodes['RefDef'].Text;
end;

procedure TXMLRELATIONType.Set_RefDef(Value: WideString);
begin
  SetAttribute('RefDef', Value);
end;

function TXMLRELATIONType.Get_Comments: WideString;
begin
  Result := AttributeNodes['Comments'].Text;
end;

procedure TXMLRELATIONType.Set_Comments(Value: WideString);
begin
  SetAttribute('Comments', Value);
end;

function TXMLRELATIONType.Get_FKRefDefIndex_Obj_id: WideString;
begin
  Result := AttributeNodes['FKRefDefIndex_Obj_id'].Text;
end;

procedure TXMLRELATIONType.Set_FKRefDefIndex_Obj_id(Value: WideString);
begin
  SetAttribute('FKRefDefIndex_Obj_id', Value);
end;

function TXMLRELATIONType.Get_Splitted: Integer;
begin
  Result := AttributeNodes['Splitted'].NodeValue;
end;

procedure TXMLRELATIONType.Set_Splitted(Value: Integer);
begin
  SetAttribute('Splitted', Value);
end;

function TXMLRELATIONType.Get_IsLinkedObject: Integer;
begin
  Result := AttributeNodes['IsLinkedObject'].NodeValue;
end;

procedure TXMLRELATIONType.Set_IsLinkedObject(Value: Integer);
begin
  SetAttribute('IsLinkedObject', Value);
end;

function TXMLRELATIONType.Get_IDLinkedModel: WideString;
begin
  Result := AttributeNodes['IDLinkedModel'].Text;
end;

procedure TXMLRELATIONType.Set_IDLinkedModel(Value: WideString);
begin
  SetAttribute('IDLinkedModel', Value);
end;

function TXMLRELATIONType.Get_Obj_id_Linked: WideString;
begin
  Result := AttributeNodes['Obj_id_Linked'].Text;
end;

procedure TXMLRELATIONType.Set_Obj_id_Linked(Value: WideString);
begin
  SetAttribute('Obj_id_Linked', Value);
end;

{ TXMLNOTESType }

procedure TXMLNOTESType.AfterConstruction;
begin
  RegisterChildNode('NOTE', TXMLNOTEType);
  ItemTag := 'NOTE';
  ItemInterface := IXMLNOTEType;
  inherited;
end;

function TXMLNOTESType.Get_NOTE(Index: Integer): IXMLNOTEType;
begin
  Result := List[Index] as IXMLNOTEType;
end;

function TXMLNOTESType.Add: IXMLNOTEType;
begin
  Result := AddItem(-1) as IXMLNOTEType;
end;

function TXMLNOTESType.Insert(const Index: Integer): IXMLNOTEType;
begin
  Result := AddItem(Index) as IXMLNOTEType;
end;

{ TXMLNOTEType }

function TXMLNOTEType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLNOTEType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLNOTEType.Get_NoteName: WideString;
begin
  Result := AttributeNodes['NoteName'].Text;
end;

procedure TXMLNOTEType.Set_NoteName(Value: WideString);
begin
  SetAttribute('NoteName', Value);
end;

function TXMLNOTEType.Get_XPos: Integer;
begin
  Result := AttributeNodes['XPos'].NodeValue;
end;

procedure TXMLNOTEType.Set_XPos(Value: Integer);
begin
  SetAttribute('XPos', Value);
end;

function TXMLNOTEType.Get_YPos: Integer;
begin
  Result := AttributeNodes['YPos'].NodeValue;
end;

procedure TXMLNOTEType.Set_YPos(Value: Integer);
begin
  SetAttribute('YPos', Value);
end;

function TXMLNOTEType.Get_NoteText: WideString;
begin
  Result := AttributeNodes['NoteText'].Text;
end;

procedure TXMLNOTEType.Set_NoteText(Value: WideString);
begin
  SetAttribute('NoteText', Value);
end;

function TXMLNOTEType.Get_IsLinkedObject: Integer;
begin
  Result := AttributeNodes['IsLinkedObject'].NodeValue;
end;

procedure TXMLNOTEType.Set_IsLinkedObject(Value: Integer);
begin
  SetAttribute('IsLinkedObject', Value);
end;

function TXMLNOTEType.Get_IDLinkedModel: WideString;
begin
  Result := AttributeNodes['IDLinkedModel'].Text;
end;

procedure TXMLNOTEType.Set_IDLinkedModel(Value: WideString);
begin
  SetAttribute('IDLinkedModel', Value);
end;

function TXMLNOTEType.Get_Obj_id_Linked: WideString;
begin
  Result := AttributeNodes['Obj_id_Linked'].Text;
end;

procedure TXMLNOTEType.Set_Obj_id_Linked(Value: WideString);
begin
  SetAttribute('Obj_id_Linked', Value);
end;

{ TXMLIMAGESType }

procedure TXMLIMAGESType.AfterConstruction;
begin
  RegisterChildNode('IMAGE', TXMLIMAGEType);
  ItemTag := 'IMAGE';
  ItemInterface := IXMLIMAGEType;
  inherited;
end;

function TXMLIMAGESType.Get_IMAGE(Index: Integer): IXMLIMAGEType;
begin
  Result := List[Index] as IXMLIMAGEType;
end;

function TXMLIMAGESType.Add: IXMLIMAGEType;
begin
  Result := AddItem(-1) as IXMLIMAGEType;
end;

function TXMLIMAGESType.Insert(const Index: Integer): IXMLIMAGEType;
begin
  Result := AddItem(Index) as IXMLIMAGEType;
end;

{ TXMLIMAGEType }

function TXMLIMAGEType.Get_ID: Integer;
begin
  Result := AttributeNodes['ID'].NodeValue;
end;

procedure TXMLIMAGEType.Set_ID(Value: Integer);
begin
  SetAttribute('ID', Value);
end;

function TXMLIMAGEType.Get_ImageName: WideString;
begin
  Result := AttributeNodes['ImageName'].Text;
end;

procedure TXMLIMAGEType.Set_ImageName(Value: WideString);
begin
  SetAttribute('ImageName', Value);
end;

function TXMLIMAGEType.Get_XPos: Integer;
begin
  Result := AttributeNodes['XPos'].NodeValue;
end;

procedure TXMLIMAGEType.Set_XPos(Value: Integer);
begin
  SetAttribute('XPos', Value);
end;

function TXMLIMAGEType.Get_YPos: Integer;
begin
  Result := AttributeNodes['YPos'].NodeValue;
end;

procedure TXMLIMAGEType.Set_YPos(Value: Integer);
begin
  SetAttribute('YPos', Value);
end;

function TXMLIMAGEType.Get_Width: Integer;
begin
  Result := AttributeNodes['Width'].NodeValue;
end;

procedure TXMLIMAGEType.Set_Width(Value: Integer);
begin
  SetAttribute('Width', Value);
end;

function TXMLIMAGEType.Get_Height: Integer;
begin
  Result := AttributeNodes['Height'].NodeValue;
end;

procedure TXMLIMAGEType.Set_Height(Value: Integer);
begin
  SetAttribute('Height', Value);
end;

function TXMLIMAGEType.Get_StrechImg: Integer;
begin
  Result := AttributeNodes['StrechImg'].NodeValue;
end;

procedure TXMLIMAGEType.Set_StrechImg(Value: Integer);
begin
  SetAttribute('StrechImg', Value);
end;

function TXMLIMAGEType.Get_ImgWidth: Integer;
begin
  Result := AttributeNodes['ImgWidth'].NodeValue;
end;

procedure TXMLIMAGEType.Set_ImgWidth(Value: Integer);
begin
  SetAttribute('ImgWidth', Value);
end;

function TXMLIMAGEType.Get_ImgHeight: Integer;
begin
  Result := AttributeNodes['ImgHeight'].NodeValue;
end;

procedure TXMLIMAGEType.Set_ImgHeight(Value: Integer);
begin
  SetAttribute('ImgHeight', Value);
end;

function TXMLIMAGEType.Get_ImgFormat: WideString;
begin
  Result := AttributeNodes['ImgFormat'].Text;
end;

procedure TXMLIMAGEType.Set_ImgFormat(Value: WideString);
begin
  SetAttribute('ImgFormat', Value);
end;

function TXMLIMAGEType.Get_ImgData: WideString;
begin
  Result := AttributeNodes['ImgData'].Text;
end;

procedure TXMLIMAGEType.Set_ImgData(Value: WideString);
begin
  SetAttribute('ImgData', Value);
end;

function TXMLIMAGEType.Get_IsLinkedObject: Integer;
begin
  Result := AttributeNodes['IsLinkedObject'].NodeValue;
end;

procedure TXMLIMAGEType.Set_IsLinkedObject(Value: Integer);
begin
  SetAttribute('IsLinkedObject', Value);
end;

function TXMLIMAGEType.Get_IDLinkedModel: WideString;
begin
  Result := AttributeNodes['IDLinkedModel'].Text;
end;

procedure TXMLIMAGEType.Set_IDLinkedModel(Value: WideString);
begin
  SetAttribute('IDLinkedModel', Value);
end;

function TXMLIMAGEType.Get_Obj_id_Linked: WideString;
begin
  Result := AttributeNodes['Obj_id_Linked'].Text;
end;

procedure TXMLIMAGEType.Set_Obj_id_Linked(Value: WideString);
begin
  SetAttribute('Obj_id_Linked', Value);
end;

{ TXMLPLUGINDATAType }

procedure TXMLPLUGINDATAType.AfterConstruction;
begin
  RegisterChildNode('PLUGINDATARECORDS', TXMLPLUGINDATARECORDSType);
  inherited;
end;

function TXMLPLUGINDATAType.Get_PLUGINDATARECORDS: IXMLPLUGINDATARECORDSType;
begin
  Result := ChildNodes['PLUGINDATARECORDS'] as IXMLPLUGINDATARECORDSType;
end;

{ TXMLPLUGINDATARECORDSType }

procedure TXMLPLUGINDATARECORDSType.AfterConstruction;
begin
  RegisterChildNode('PLUGINDATARECORD', TXMLPLUGINDATARECORDType);
  ItemTag := 'PLUGINDATARECORD';
  ItemInterface := IXMLPLUGINDATARECORDType;
  inherited;
end;

function TXMLPLUGINDATARECORDSType.Get_PLUGINDATARECORD(Index: Integer): IXMLPLUGINDATARECORDType;
begin
  Result := List[Index] as IXMLPLUGINDATARECORDType;
end;

function TXMLPLUGINDATARECORDSType.Add: IXMLPLUGINDATARECORDType;
begin
  Result := AddItem(-1) as IXMLPLUGINDATARECORDType;
end;

function TXMLPLUGINDATARECORDSType.Insert(const Index: Integer): IXMLPLUGINDATARECORDType;
begin
  Result := AddItem(Index) as IXMLPLUGINDATARECORDType;
end;

{ TXMLPLUGINDATARECORDType }

procedure TXMLPLUGINDATARECORDType.AfterConstruction;
begin
  RegisterChildNode('PLUGINDATAPARAMS', TXMLPLUGINDATAPARAMSType);
  inherited;
end;

function TXMLPLUGINDATARECORDType.Get_PluginName: WideString;
begin
  Result := AttributeNodes['PluginName'].Text;
end;

procedure TXMLPLUGINDATARECORDType.Set_PluginName(Value: WideString);
begin
  SetAttribute('PluginName', Value);
end;

function TXMLPLUGINDATARECORDType.Get_Obj_id: WideString;
begin
  Result := AttributeNodes['Obj_id'].Text;
end;

procedure TXMLPLUGINDATARECORDType.Set_Obj_id(Value: WideString);
begin
  SetAttribute('Obj_id', Value);
end;

function TXMLPLUGINDATARECORDType.Get_DataValue: WideString;
begin
  Result := AttributeNodes['DataValue'].Text;
end;

procedure TXMLPLUGINDATARECORDType.Set_DataValue(Value: WideString);
begin
  SetAttribute('DataValue', Value);
end;

function TXMLPLUGINDATARECORDType.Get_PLUGINDATAPARAMS: IXMLPLUGINDATAPARAMSType;
begin
  Result := ChildNodes['PLUGINDATAPARAMS'] as IXMLPLUGINDATAPARAMSType;
end;

{ TXMLPLUGINDATAPARAMSType }

procedure TXMLPLUGINDATAPARAMSType.AfterConstruction;
begin
  RegisterChildNode('PLUGINDATAPARAM', TXMLPLUGINDATAPARAMType);
  ItemTag := 'PLUGINDATAPARAM';
  ItemInterface := IXMLPLUGINDATAPARAMType;
  inherited;
end;

function TXMLPLUGINDATAPARAMSType.Get_PLUGINDATAPARAM(Index: Integer): IXMLPLUGINDATAPARAMType;
begin
  Result := List[Index] as IXMLPLUGINDATAPARAMType;
end;

function TXMLPLUGINDATAPARAMSType.Add: IXMLPLUGINDATAPARAMType;
begin
  Result := AddItem(-1) as IXMLPLUGINDATAPARAMType;
end;

function TXMLPLUGINDATAPARAMSType.Insert(const Index: Integer): IXMLPLUGINDATAPARAMType;
begin
  Result := AddItem(Index) as IXMLPLUGINDATAPARAMType;
end;

{ TXMLPLUGINDATAPARAMType }

function TXMLPLUGINDATAPARAMType.Get_Value: WideString;
begin
  Result := AttributeNodes['Value'].Text;
end;

procedure TXMLPLUGINDATAPARAMType.Set_Value(Value: WideString);
begin
  SetAttribute('Value', Value);
end;

{ TXMLQUERYDATAType }

procedure TXMLQUERYDATAType.AfterConstruction;
begin
  RegisterChildNode('QUERYRECORDS', TXMLQUERYRECORDSType);
  inherited;
end;

function TXMLQUERYDATAType.Get_QUERYRECORDS: IXMLQUERYRECORDSType;
begin
  Result := ChildNodes['QUERYRECORDS'] as IXMLQUERYRECORDSType;
end;

{ TXMLQUERYRECORDSType }

procedure TXMLQUERYRECORDSType.AfterConstruction;
begin
  RegisterChildNode('QUERYRECORD', TXMLQUERYRECORDType);
  ItemTag := 'QUERYRECORD';
  ItemInterface := IXMLQUERYRECORDType;
  inherited;
end;

function TXMLQUERYRECORDSType.Get_QUERYRECORD(Index: Integer): IXMLQUERYRECORDType;
begin
  Result := List[Index] as IXMLQUERYRECORDType;
end;

function TXMLQUERYRECORDSType.Add: IXMLQUERYRECORDType;
begin
  Result := AddItem(-1) as IXMLQUERYRECORDType;
end;

function TXMLQUERYRECORDSType.Insert(const Index: Integer): IXMLQUERYRECORDType;
begin
  Result := AddItem(Index) as IXMLQUERYRECORDType;
end;

{ TXMLQUERYRECORDType }

function TXMLQUERYRECORDType.Get_SQLCmdType: Integer;
begin
  Result := AttributeNodes['SQLCmdType'].NodeValue;
end;

procedure TXMLQUERYRECORDType.Set_SQLCmdType(Value: Integer);
begin
  SetAttribute('SQLCmdType', Value);
end;

function TXMLQUERYRECORDType.Get_StoredPosition: WideString;
begin
  Result := AttributeNodes['StoredPosition'].Text;
end;

procedure TXMLQUERYRECORDType.Set_StoredPosition(Value: WideString);
begin
  SetAttribute('StoredPosition', Value);
end;

function TXMLQUERYRECORDType.Get_SQLText: WideString;
begin
  Result := AttributeNodes['SQLText'].Text;
end;

procedure TXMLQUERYRECORDType.Set_SQLText(Value: WideString);
begin
  SetAttribute('SQLText', Value);
end;

{ TXMLLINKEDMODELSType }

procedure TXMLLINKEDMODELSType.AfterConstruction;
begin
  RegisterChildNode('LINKEDMODEL', TXMLLINKEDMODELType);
  ItemTag := 'LINKEDMODEL';
  ItemInterface := IXMLLINKEDMODELType;
  inherited;
end;

function TXMLLINKEDMODELSType.Get_LINKEDMODEL(Index: Integer): IXMLLINKEDMODELType;
begin
  Result := List[Index] as IXMLLINKEDMODELType;
end;

function TXMLLINKEDMODELSType.Add: IXMLLINKEDMODELType;
begin
  Result := AddItem(-1) as IXMLLINKEDMODELType;
end;

function TXMLLINKEDMODELSType.Insert(const Index: Integer): IXMLLINKEDMODELType;
begin
  Result := AddItem(Index) as IXMLLINKEDMODELType;
end;

{ TXMLLINKEDMODELType }

function TXMLLINKEDMODELType.Get_IDLinkedModel: Integer;
begin
  Result := AttributeNodes['IDLinkedModel'].NodeValue;
end;

procedure TXMLLINKEDMODELType.Set_IDLinkedModel(Value: Integer);
begin
  SetAttribute('IDLinkedModel', Value);
end;

function TXMLLINKEDMODELType.Get_ModelName: WideString;
begin
  Result := AttributeNodes['ModelName'].Text;
end;

procedure TXMLLINKEDMODELType.Set_ModelName(Value: WideString);
begin
  SetAttribute('ModelName', Value);
end;

function TXMLLINKEDMODELType.Get_IDModel: Integer;
begin
  Result := AttributeNodes['IDModel'].NodeValue;
end;

procedure TXMLLINKEDMODELType.Set_IDModel(Value: Integer);
begin
  SetAttribute('IDModel', Value);
end;

function TXMLLINKEDMODELType.Get_IsStoredInDB: Integer;
begin
  Result := AttributeNodes['IsStoredInDB'].NodeValue;
end;

procedure TXMLLINKEDMODELType.Set_IsStoredInDB(Value: Integer);
begin
  SetAttribute('IsStoredInDB', Value);
end;

function TXMLLINKEDMODELType.Get_ModelFilename: WideString;
begin
  Result := AttributeNodes['ModelFilename'].Text;
end;

procedure TXMLLINKEDMODELType.Set_ModelFilename(Value: WideString);
begin
  SetAttribute('ModelFilename', Value);
end;

function TXMLLINKEDMODELType.Get_DriverName: WideString;
begin
  Result := AttributeNodes['DriverName'].Text;
end;

procedure TXMLLINKEDMODELType.Set_DriverName(Value: WideString);
begin
  SetAttribute('DriverName', Value);
end;

function TXMLLINKEDMODELType.Get_DBConnName: WideString;
begin
  Result := AttributeNodes['DBConnName'].Text;
end;

procedure TXMLLINKEDMODELType.Set_DBConnName(Value: WideString);
begin
  SetAttribute('DBConnName', Value);
end;

function TXMLLINKEDMODELType.Get_HostCaption: WideString;
begin
  Result := AttributeNodes['HostCaption'].Text;
end;

procedure TXMLLINKEDMODELType.Set_HostCaption(Value: WideString);
begin
  SetAttribute('HostCaption', Value);
end;

function TXMLLINKEDMODELType.Get_HostName: WideString;
begin
  Result := AttributeNodes['HostName'].Text;
end;

procedure TXMLLINKEDMODELType.Set_HostName(Value: WideString);
begin
  SetAttribute('HostName', Value);
end;

function TXMLLINKEDMODELType.Get_Database: WideString;
begin
  Result := AttributeNodes['Database'].Text;
end;

procedure TXMLLINKEDMODELType.Set_Database(Value: WideString);
begin
  SetAttribute('Database', Value);
end;

function TXMLLINKEDMODELType.Get_User: WideString;
begin
  Result := AttributeNodes['User'].Text;
end;

procedure TXMLLINKEDMODELType.Set_User(Value: WideString);
begin
  SetAttribute('User', Value);
end;

end.
