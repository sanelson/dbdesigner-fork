unit EERModel_XML_ERwin41_Import;

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
// Unit EERModel_XML_ERwin41_Import.pas
// ------------------------------------
// Version 1.0, 03.05.2003, Mike
// Description
//   Contains the XML Data Binding for the ERwin41 Import
//
// Changes:
//   Version 1.0, 03.05.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLERwin4Type = interface;
  IXMLModelType = interface;
  IXMLModelPropsType = interface;
  IXMLType_ = interface;
  IXMLFile_NameType = interface;
  IXMLFile_FormatType = interface;
  IXMLTarget_ServerType = interface;
  IXMLDBMS_VersionType = interface;
  IXMLDBMS_Minor_VersionType = interface;
  IXMLEntity_GroupsType = interface;
  IXMLEntityType = interface;
  IXMLEntityPropsType = interface;
  IXMLAttribute_GroupsType = interface;
  IXMLAttributeType = interface;
  IXMLAttributePropsType = interface;
  IXMLMaster_AttributeType = interface;
  IXMLHistory_Information_GroupsType = interface;
  IXMLHistory_InformationType = interface;
  IXMLHistory_InformationPropsType = interface;
  IXMLDefault_Object_Alternative_IdType = interface;
  IXMLKey_Group_GroupsType = interface;
  IXMLKey_GroupType = interface;
  IXMLKey_GroupPropsType = interface;
  IXMLKey_Group_Member_GroupsType = interface;
  IXMLKey_Group_MemberType = interface;
  IXMLKey_Group_MemberPropsType = interface;
  IXMLDomain_GroupsType = interface;
  IXMLDomainType = interface;
  IXMLDomainPropsType = interface;
  IXMLPhysical_Domain_NameType = interface;
  IXMLParent_DomainType = interface;
  IXMLBitmap_GroupsType = interface;
  IXMLBitmapType = interface;
  IXMLBitmapPropsType = interface;
  IXMLBitmap_DIB_SizeType = interface;
  IXMLBitmap_DIB_HandleType = interface;
  IXMLFlagsType = interface;
  IXMLImage_IndexType = interface;
  IXMLSubject_Area_GroupsType = interface;
  IXMLSubject_AreaType = interface;
  IXMLSubject_AreaPropsType = interface;
  IXMLNameType = interface;
  IXMLReferenced_EntitiesType = interface;
  IXMLReferenced_EntitiesTypeList = interface;
  IXMLReferenced_RelationshipsType = interface;
  IXMLReferenced_RelationshipsTypeList = interface;
  IXMLStored_Display_GroupsType = interface;
  IXMLStored_DisplayType = interface;
  IXMLStored_DisplayPropsType = interface;
  IXMLDrawing_Object_Entity_GroupsType = interface;
  IXMLDrawing_Object_EntityType = interface;
  IXMLDrawing_Object_EntityPropsType = interface;
  IXMLDrawing_Object_Relationship_GroupsType = interface;
  IXMLDrawing_Object_RelationshipType = interface;
  IXMLDrawing_Object_RelationshipPropsType = interface;
  IXMLRelationship_GroupsType = interface;
  IXMLRelationshipType = interface;
  IXMLRelationshipPropsType = interface;
  IXMLDefault_Value_GroupsType = interface;
  IXMLDefault_ValueType = interface;
  IXMLDefault_ValuePropsType = interface;
  IXMLTrigger_Template_GroupsType = interface;
  IXMLTrigger_TemplateType = interface;
  IXMLTrigger_TemplatePropsType = interface;
  IXMLTemplate_CodeType = interface;
  IXMLDefault_Trigger_Template_GroupsType = interface;
  IXMLDefault_Trigger_TemplateType = interface;
  IXMLDefault_Trigger_TemplatePropsType = interface;
  IXMLTemplate_Purpose_TextType = interface;
  IXMLTemplate_PurposeType = interface;
  IXMLName_Mapping_GroupsType = interface;
  IXMLName_MappingType = interface;
  IXMLName_MappingPropsType = interface;
  IXMLNaming_Options_GroupsType = interface;
  IXMLNaming_OptionsType = interface;
  IXMLNaming_OptionsPropsType = interface;
  IXMLPhysical_OnlyType = interface;
  IXMLLogical_OnlyType = interface;
  IXMLString_List = interface;

{ IXMLERwin4Type }

  IXMLERwin4Type = interface(IXMLNode)
    ['{86FB8222-FC26-4D47-B8DF-C0FC451C4C67}']
    { Property Accessors }
    function Get_FileVersion: Integer;
    function Get_Model: IXMLModelType;
    procedure Set_FileVersion(Value: Integer);
    { Methods & Properties }
    property FileVersion: Integer read Get_FileVersion write Set_FileVersion;
    property Model: IXMLModelType read Get_Model;
  end;

{ IXMLModelType }

  IXMLModelType = interface(IXMLNode)
    ['{904B28E4-8D8F-407C-B353-3086C74CAD78}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_ModelType: Integer;
    function Get_TargetServer: Integer;
    function Get_DBMSVersion: Integer;
    function Get_DBMSMinorVersion: Integer;
    function Get_ModelProps: IXMLModelPropsType;
    function Get_Entity_Groups: IXMLEntity_GroupsType;
    function Get_Domain_Groups: IXMLDomain_GroupsType;
    function Get_Bitmap_Groups: IXMLBitmap_GroupsType;
    function Get_Subject_Area_Groups: IXMLSubject_Area_GroupsType;
    function Get_Relationship_Groups: IXMLRelationship_GroupsType;
    function Get_Default_Value_Groups: IXMLDefault_Value_GroupsType;
    function Get_Trigger_Template_Groups: IXMLTrigger_Template_GroupsType;
    function Get_Default_Trigger_Template_Groups: IXMLDefault_Trigger_Template_GroupsType;
    function Get_Name_Mapping_Groups: IXMLName_Mapping_GroupsType;
    function Get_Naming_Options_Groups: IXMLNaming_Options_GroupsType;
    function Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_ModelType(Value: Integer);
    procedure Set_TargetServer(Value: Integer);
    procedure Set_DBMSVersion(Value: Integer);
    procedure Set_DBMSMinorVersion(Value: Integer);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property ModelType: Integer read Get_ModelType write Set_ModelType;
    property TargetServer: Integer read Get_TargetServer write Set_TargetServer;
    property DBMSVersion: Integer read Get_DBMSVersion write Set_DBMSVersion;
    property DBMSMinorVersion: Integer read Get_DBMSMinorVersion write Set_DBMSMinorVersion;
    property ModelProps: IXMLModelPropsType read Get_ModelProps;
    property Entity_Groups: IXMLEntity_GroupsType read Get_Entity_Groups;
    property Domain_Groups: IXMLDomain_GroupsType read Get_Domain_Groups;
    property Bitmap_Groups: IXMLBitmap_GroupsType read Get_Bitmap_Groups;
    property Subject_Area_Groups: IXMLSubject_Area_GroupsType read Get_Subject_Area_Groups;
    property Relationship_Groups: IXMLRelationship_GroupsType read Get_Relationship_Groups;
    property Default_Value_Groups: IXMLDefault_Value_GroupsType read Get_Default_Value_Groups;
    property Trigger_Template_Groups: IXMLTrigger_Template_GroupsType read Get_Trigger_Template_Groups;
    property Default_Trigger_Template_Groups: IXMLDefault_Trigger_Template_GroupsType read Get_Default_Trigger_Template_Groups;
    property Name_Mapping_Groups: IXMLName_Mapping_GroupsType read Get_Name_Mapping_Groups;
    property Naming_Options_Groups: IXMLNaming_Options_GroupsType read Get_Naming_Options_Groups;
    property History_Information_Groups: IXMLHistory_Information_GroupsType read Get_History_Information_Groups;
  end;

{ IXMLModelPropsType }

  IXMLModelPropsType = interface(IXMLNode)
    ['{171957A4-167A-4A8A-A21D-35BE42537D7B}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Type_: IXMLType_;
    function Get_Tracking_History_Objects: WideString;
    function Get_Tracking_History_Events: WideString;
    function Get_File_Name: IXMLFile_NameType;
    function Get_Page_Grid: Integer;
    function Get_File_Format: IXMLFile_FormatType;
    function Get_Entity_Width: Integer;
    function Get_Entity_Height: Integer;
    function Get_Layout_Grid: Integer;
    function Get_Layout_Grid_X: Integer;
    function Get_Layout_Grid_Y: Integer;
    function Get_Font_Height: Integer;
    function Get_Font_Width: Integer;
    function Get_Unique_Names: Integer;
    function Get_Target_Server: IXMLTarget_ServerType;
    function Get_Default_Datatype: WideString;
    function Get_Non_Key_Null: Integer;
    function Get_Default_Fonts_and_Colors: WideString;
    function Get_Repos: Integer;
    function Get_DBMS_Version: IXMLDBMS_VersionType;
    function Get_Logical_Notation: Integer;
    function Get_DBMS_Minor_Version: IXMLDBMS_Minor_VersionType;
    function Get_Old_Repos: Integer;
    function Get_Max_View_Expr_Display_Len: Integer;
    function Get_Physical_Notation: Integer;
    function Get_Index_Name_Macro: WideString;
    function Get_Max_Def_Display_Len: Integer;
    function Get_Table_Name_Macro: WideString;
    function Get_Current_Tool: WideString;
    function Get_MM_Preview: WideString;
    function Get_Saved_From_Previous_Version: WideString;
    function Get_WYSIWYG_Print: WideString;
    function Get_Model_Background_Color: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Tracking_History_Objects(Value: WideString);
    procedure Set_Tracking_History_Events(Value: WideString);
    procedure Set_Page_Grid(Value: Integer);
    procedure Set_Entity_Width(Value: Integer);
    procedure Set_Entity_Height(Value: Integer);
    procedure Set_Layout_Grid(Value: Integer);
    procedure Set_Layout_Grid_X(Value: Integer);
    procedure Set_Layout_Grid_Y(Value: Integer);
    procedure Set_Font_Height(Value: Integer);
    procedure Set_Font_Width(Value: Integer);
    procedure Set_Unique_Names(Value: Integer);
    procedure Set_Default_Datatype(Value: WideString);
    procedure Set_Non_Key_Null(Value: Integer);
    procedure Set_Default_Fonts_and_Colors(Value: WideString);
    procedure Set_Repos(Value: Integer);
    procedure Set_Logical_Notation(Value: Integer);
    procedure Set_Old_Repos(Value: Integer);
    procedure Set_Max_View_Expr_Display_Len(Value: Integer);
    procedure Set_Physical_Notation(Value: Integer);
    procedure Set_Index_Name_Macro(Value: WideString);
    procedure Set_Max_Def_Display_Len(Value: Integer);
    procedure Set_Table_Name_Macro(Value: WideString);
    procedure Set_Current_Tool(Value: WideString);
    procedure Set_MM_Preview(Value: WideString);
    procedure Set_Saved_From_Previous_Version(Value: WideString);
    procedure Set_WYSIWYG_Print(Value: WideString);
    procedure Set_Model_Background_Color(Value: Integer);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Type_: IXMLType_ read Get_Type_;
    property Tracking_History_Objects: WideString read Get_Tracking_History_Objects write Set_Tracking_History_Objects;
    property Tracking_History_Events: WideString read Get_Tracking_History_Events write Set_Tracking_History_Events;
    property File_Name: IXMLFile_NameType read Get_File_Name;
    property Page_Grid: Integer read Get_Page_Grid write Set_Page_Grid;
    property File_Format: IXMLFile_FormatType read Get_File_Format;
    property Entity_Width: Integer read Get_Entity_Width write Set_Entity_Width;
    property Entity_Height: Integer read Get_Entity_Height write Set_Entity_Height;
    property Layout_Grid: Integer read Get_Layout_Grid write Set_Layout_Grid;
    property Layout_Grid_X: Integer read Get_Layout_Grid_X write Set_Layout_Grid_X;
    property Layout_Grid_Y: Integer read Get_Layout_Grid_Y write Set_Layout_Grid_Y;
    property Font_Height: Integer read Get_Font_Height write Set_Font_Height;
    property Font_Width: Integer read Get_Font_Width write Set_Font_Width;
    property Unique_Names: Integer read Get_Unique_Names write Set_Unique_Names;
    property Target_Server: IXMLTarget_ServerType read Get_Target_Server;
    property Default_Datatype: WideString read Get_Default_Datatype write Set_Default_Datatype;
    property Non_Key_Null: Integer read Get_Non_Key_Null write Set_Non_Key_Null;
    property Default_Fonts_and_Colors: WideString read Get_Default_Fonts_and_Colors write Set_Default_Fonts_and_Colors;
    property Repos: Integer read Get_Repos write Set_Repos;
    property DBMS_Version: IXMLDBMS_VersionType read Get_DBMS_Version;
    property Logical_Notation: Integer read Get_Logical_Notation write Set_Logical_Notation;
    property DBMS_Minor_Version: IXMLDBMS_Minor_VersionType read Get_DBMS_Minor_Version;
    property Old_Repos: Integer read Get_Old_Repos write Set_Old_Repos;
    property Max_View_Expr_Display_Len: Integer read Get_Max_View_Expr_Display_Len write Set_Max_View_Expr_Display_Len;
    property Physical_Notation: Integer read Get_Physical_Notation write Set_Physical_Notation;
    property Index_Name_Macro: WideString read Get_Index_Name_Macro write Set_Index_Name_Macro;
    property Max_Def_Display_Len: Integer read Get_Max_Def_Display_Len write Set_Max_Def_Display_Len;
    property Table_Name_Macro: WideString read Get_Table_Name_Macro write Set_Table_Name_Macro;
    property Current_Tool: WideString read Get_Current_Tool write Set_Current_Tool;
    property MM_Preview: WideString read Get_MM_Preview write Set_MM_Preview;
    property Saved_From_Previous_Version: WideString read Get_Saved_From_Previous_Version write Set_Saved_From_Previous_Version;
    property WYSIWYG_Print: WideString read Get_WYSIWYG_Print write Set_WYSIWYG_Print;
    property Model_Background_Color: Integer read Get_Model_Background_Color write Set_Model_Background_Color;
  end;

{ IXMLType_ }

  IXMLType_ = interface(IXMLNode)
    ['{1AFE674B-0AD0-4DA8-B029-0389ADE40163}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLFile_NameType }

  IXMLFile_NameType = interface(IXMLNode)
    ['{28F719D9-481D-427B-9377-8618F95A5081}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLFile_FormatType }

  IXMLFile_FormatType = interface(IXMLNode)
    ['{559BC9B1-BBDD-42EA-8B02-7365C4F05972}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLTarget_ServerType }

  IXMLTarget_ServerType = interface(IXMLNode)
    ['{D606093B-957F-41F1-8A63-501C76D25BF7}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLDBMS_VersionType }

  IXMLDBMS_VersionType = interface(IXMLNode)
    ['{38A9FC77-8672-4A4A-8707-7A4BFE6F235F}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLDBMS_Minor_VersionType }

  IXMLDBMS_Minor_VersionType = interface(IXMLNode)
    ['{ED770888-D35E-4FD5-8388-F62AB7996C4D}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLEntity_GroupsType }

  IXMLEntity_GroupsType = interface(IXMLNodeCollection)
    ['{DB0E8349-17E0-4B8F-B726-AB17B39A5321}']
    { Property Accessors }
    function Get_Entity(Index: Integer): IXMLEntityType;
    { Methods & Properties }
    function Add: IXMLEntityType;
    function Insert(const Index: Integer): IXMLEntityType;
    property Entity[Index: Integer]: IXMLEntityType read Get_Entity; default;
  end;

{ IXMLEntityType }

  IXMLEntityType = interface(IXMLNode)
    ['{06EF027F-B1E2-4E3A-89DA-9316A7057E73}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_EntityProps: IXMLEntityPropsType;
    function Get_Attribute_Groups: IXMLAttribute_GroupsType;
    function Get_Key_Group_Groups: IXMLKey_Group_GroupsType;
    function Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property EntityProps: IXMLEntityPropsType read Get_EntityProps;
    property Attribute_Groups: IXMLAttribute_GroupsType read Get_Attribute_Groups;
    property Key_Group_Groups: IXMLKey_Group_GroupsType read Get_Key_Group_Groups;
    property History_Information_Groups: IXMLHistory_Information_GroupsType read Get_History_Information_Groups;
  end;

{ IXMLEntityPropsType }

  IXMLEntityPropsType = interface(IXMLNode)
    ['{EE4C7B96-8F42-4BA4-BE95-58B127E517E9}']
    { Property Accessors }
    function Get_Type_: Integer;
    function Get_Index_Generate: Integer;
    function Get_Physical_Name: WideString;
    function Get_Comment: WideString;
    function Get_Physical_Only: WideString;
    procedure Set_Type_(Value: Integer);
    procedure Set_Index_Generate(Value: Integer);
    procedure Set_Physical_Name(Value: WideString);
    procedure Set_Comment(Value: WideString);
    procedure Set_Physical_Only(Value: WideString);
    { Methods & Properties }
    property Type_: Integer read Get_Type_ write Set_Type_;
    property Index_Generate: Integer read Get_Index_Generate write Set_Index_Generate;
    property Physical_Name: WideString read Get_Physical_Name write Set_Physical_Name;
    property Comment: WideString read Get_Comment write Set_Comment;
    property Physical_Only: WideString read Get_Physical_Only write Set_Physical_Only;
  end;

{ IXMLAttribute_GroupsType }

  IXMLAttribute_GroupsType = interface(IXMLNodeCollection)
    ['{C775CFDB-DD32-4551-B486-DB633BACCB51}']
    { Property Accessors }
    function Get_Attribute(Index: Integer): IXMLAttributeType;
    { Methods & Properties }
    function Add: IXMLAttributeType;
    function Insert(const Index: Integer): IXMLAttributeType;
    property Attribute[Index: Integer]: IXMLAttributeType read Get_Attribute; default;
  end;

{ IXMLAttributeType }

  IXMLAttributeType = interface(IXMLNode)
    ['{AA7A2A5F-1EDB-4F7D-97BA-183BC4EBE55B}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_AttributeProps: IXMLAttributePropsType;
    function Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property AttributeProps: IXMLAttributePropsType read Get_AttributeProps;
    property History_Information_Groups: IXMLHistory_Information_GroupsType read Get_History_Information_Groups;
  end;

{ IXMLAttributePropsType }

  IXMLAttributePropsType = interface(IXMLNode)
    ['{E217454F-8E51-40DB-8F80-0D16445DD51B}']
    { Property Accessors }
    function Get_Type_: Integer;
    function Get_Null_Option: Integer;
    function Get_Order: Integer;
    function Get_Physical_Order: Integer;
    function Get_Parent_Attribute: WideString;
    function Get_Parent_Relationship: WideString;
    function Get_Physical_Name: WideString;
    function Get_Parent_Domain: WideString;
    function Get_Hide_in_Logical: WideString;
    function Get_Hide_in_Physical: WideString;
    function Get_Master_Attribute: IXMLMaster_AttributeType;
    function Get_DO_Color_Inherited: WideString;
    function Get_DO_Font_Inherited: WideString;
    function Get_Datatype: WideString;
    function Get_Comment: WideString;
    function Get_Default: WideString;
    procedure Set_Type_(Value: Integer);
    procedure Set_Null_Option(Value: Integer);
    procedure Set_Order(Value: Integer);
    procedure Set_Physical_Order(Value: Integer);
    procedure Set_Parent_Attribute(Value: WideString);
    procedure Set_Parent_Relationship(Value: WideString);
    procedure Set_Physical_Name(Value: WideString);
    procedure Set_Parent_Domain(Value: WideString);
    procedure Set_Hide_in_Logical(Value: WideString);
    procedure Set_Hide_in_Physical(Value: WideString);
    procedure Set_DO_Color_Inherited(Value: WideString);
    procedure Set_DO_Font_Inherited(Value: WideString);
    procedure Set_Datatype(Value: WideString);
    procedure Set_Comment(Value: WideString);
    procedure Set_Default(Value: WideString);
    { Methods & Properties }
    property Type_: Integer read Get_Type_ write Set_Type_;
    property Null_Option: Integer read Get_Null_Option write Set_Null_Option;
    property Order: Integer read Get_Order write Set_Order;
    property Physical_Order: Integer read Get_Physical_Order write Set_Physical_Order;
    property Parent_Attribute: WideString read Get_Parent_Attribute write Set_Parent_Attribute;
    property Parent_Relationship: WideString read Get_Parent_Relationship write Set_Parent_Relationship;
    property Physical_Name: WideString read Get_Physical_Name write Set_Physical_Name;
    property Parent_Domain: WideString read Get_Parent_Domain write Set_Parent_Domain;
    property Hide_in_Logical: WideString read Get_Hide_in_Logical write Set_Hide_in_Logical;
    property Hide_in_Physical: WideString read Get_Hide_in_Physical write Set_Hide_in_Physical;
    property Master_Attribute: IXMLMaster_AttributeType read Get_Master_Attribute;
    property DO_Color_Inherited: WideString read Get_DO_Color_Inherited write Set_DO_Color_Inherited;
    property DO_Font_Inherited: WideString read Get_DO_Font_Inherited write Set_DO_Font_Inherited;
    property Datatype: WideString read Get_Datatype write Set_Datatype;
    property Comment: WideString read Get_Comment write Set_Comment;
    property Default: WideString read Get_Default write Set_Default;
  end;

{ IXMLMaster_AttributeType }

  IXMLMaster_AttributeType = interface(IXMLNode)
    ['{E8A91116-7F09-475A-86AD-369295316C15}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLHistory_Information_GroupsType }

  IXMLHistory_Information_GroupsType = interface(IXMLNode)
    ['{0BA1212D-808F-42DA-8E64-ACD6880090EF}']
    { Property Accessors }
    function Get_History_Information: IXMLHistory_InformationType;
    { Methods & Properties }
    property History_Information: IXMLHistory_InformationType read Get_History_Information;
  end;

{ IXMLHistory_InformationType }

  IXMLHistory_InformationType = interface(IXMLNode)
    ['{656A5AA2-CC49-4B7B-95C9-0CD317AA9F8A}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_History_InformationProps: IXMLHistory_InformationPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property History_InformationProps: IXMLHistory_InformationPropsType read Get_History_InformationProps;
  end;

{ IXMLHistory_InformationPropsType }

  IXMLHistory_InformationPropsType = interface(IXMLNode)
    ['{36E655F6-F11A-410E-8C64-16FC2D28082A}']
    { Property Accessors }
    function Get_Type_: Integer;
    function Get_Created_Time: Integer;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Type_(Value: Integer);
    procedure Set_Created_Time(Value: Integer);
    { Methods & Properties }
    property Type_: Integer read Get_Type_ write Set_Type_;
    property Created_Time: Integer read Get_Created_Time write Set_Created_Time;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
  end;

{ IXMLDefault_Object_Alternative_IdType }

  IXMLDefault_Object_Alternative_IdType = interface(IXMLNode)
    ['{6038EA0D-9420-4600-A207-DD628E925C0E}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLKey_Group_GroupsType }

  IXMLKey_Group_GroupsType = interface(IXMLNodeCollection)
    ['{25B661FB-6AF4-4CFD-85CF-E10E0BCE0CA0}']
    { Property Accessors }
    function Get_Key_Group(Index: Integer): IXMLKey_GroupType;
    { Methods & Properties }
    function Add: IXMLKey_GroupType;
    function Insert(const Index: Integer): IXMLKey_GroupType;
    property Key_Group[Index: Integer]: IXMLKey_GroupType read Get_Key_Group; default;
  end;

{ IXMLKey_GroupType }

  IXMLKey_GroupType = interface(IXMLNode)
    ['{996CD7CC-58C1-4B8B-ABF9-908D2F81CF07}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Key_GroupProps: IXMLKey_GroupPropsType;
    function Get_Key_Group_Member_Groups: IXMLKey_Group_Member_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Key_GroupProps: IXMLKey_GroupPropsType read Get_Key_GroupProps;
    property Key_Group_Member_Groups: IXMLKey_Group_Member_GroupsType read Get_Key_Group_Member_Groups;
  end;

{ IXMLKey_GroupPropsType }

  IXMLKey_GroupPropsType = interface(IXMLNode)
    ['{878D021E-A69C-477F-B821-8BAED7A9FEE5}']
    { Property Accessors }
    function Get_Key_Group_Type: WideString;
    function Get_Index_Generate: Integer;
    function Get_Key_Group_Relationship_Pointer: WideString;
    function Get_ORACLE_BITMAP: WideString;
    function Get_Index_Clustered: Integer;
    function Get_Partition_Global_Type: WideString;
    function Get_Physical_Name: WideString;
    procedure Set_Key_Group_Type(Value: WideString);
    procedure Set_Index_Generate(Value: Integer);
    procedure Set_Key_Group_Relationship_Pointer(Value: WideString);
    procedure Set_ORACLE_BITMAP(Value: WideString);
    procedure Set_Index_Clustered(Value: Integer);
    procedure Set_Partition_Global_Type(Value: WideString);
    procedure Set_Physical_Name(Value: WideString);
    { Methods & Properties }
    property Key_Group_Type: WideString read Get_Key_Group_Type write Set_Key_Group_Type;
    property Index_Generate: Integer read Get_Index_Generate write Set_Index_Generate;
    property Key_Group_Relationship_Pointer: WideString read Get_Key_Group_Relationship_Pointer write Set_Key_Group_Relationship_Pointer;
    property ORACLE_BITMAP: WideString read Get_ORACLE_BITMAP write Set_ORACLE_BITMAP;
    property Index_Clustered: Integer read Get_Index_Clustered write Set_Index_Clustered;
    property Partition_Global_Type: WideString read Get_Partition_Global_Type write Set_Partition_Global_Type;
    property Physical_Name: WideString read Get_Physical_Name write Set_Physical_Name;
  end;

{ IXMLKey_Group_Member_GroupsType }

  IXMLKey_Group_Member_GroupsType = interface(IXMLNodeCollection)
    ['{9C6825D3-BF9D-4D26-89E8-E5B26BB01FF5}']
    { Property Accessors }
    function Get_Key_Group_Member(Index: Integer): IXMLKey_Group_MemberType;
    { Methods & Properties }
    function Add: IXMLKey_Group_MemberType;
    function Insert(const Index: Integer): IXMLKey_Group_MemberType;
    property Key_Group_Member[Index: Integer]: IXMLKey_Group_MemberType read Get_Key_Group_Member; default;
  end;

{ IXMLKey_Group_MemberType }

  IXMLKey_Group_MemberType = interface(IXMLNode)
    ['{8F59C4FA-4034-41EB-885C-85C4F7DA7D7B}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: Integer;
    function Get_Key_Group_MemberProps: IXMLKey_Group_MemberPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: Integer);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: Integer read Get_Name write Set_Name;
    property Key_Group_MemberProps: IXMLKey_Group_MemberPropsType read Get_Key_Group_MemberProps;
  end;

{ IXMLKey_Group_MemberPropsType }

  IXMLKey_Group_MemberPropsType = interface(IXMLNode)
    ['{B6B9E306-A50B-46ED-A7C7-B4C5E4E81C4F}']
    { Property Accessors }
    function Get_Key_Group_Member_Column: WideString;
    function Get_Key_Group_Sort_Order: WideString;
    function Get_Key_Group_Position: Integer;
    procedure Set_Key_Group_Member_Column(Value: WideString);
    procedure Set_Key_Group_Sort_Order(Value: WideString);
    procedure Set_Key_Group_Position(Value: Integer);
    { Methods & Properties }
    property Key_Group_Member_Column: WideString read Get_Key_Group_Member_Column write Set_Key_Group_Member_Column;
    property Key_Group_Sort_Order: WideString read Get_Key_Group_Sort_Order write Set_Key_Group_Sort_Order;
    property Key_Group_Position: Integer read Get_Key_Group_Position write Set_Key_Group_Position;
  end;

{ IXMLDomain_GroupsType }

  IXMLDomain_GroupsType = interface(IXMLNodeCollection)
    ['{ADFF59BD-A685-4CCE-BC27-BC62F53143F1}']
    { Property Accessors }
    function Get_Domain(Index: Integer): IXMLDomainType;
    { Methods & Properties }
    function Add: IXMLDomainType;
    function Insert(const Index: Integer): IXMLDomainType;
    property Domain[Index: Integer]: IXMLDomainType read Get_Domain; default;
  end;

{ IXMLDomainType }

  IXMLDomainType = interface(IXMLNode)
    ['{561F59DE-7220-435C-9B01-44E9F327FEB5}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_DomainProps: IXMLDomainPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property DomainProps: IXMLDomainPropsType read Get_DomainProps;
  end;

{ IXMLDomainPropsType }

  IXMLDomainPropsType = interface(IXMLNode)
    ['{47514773-3C23-45CD-B8BF-362446BA7DFB}']
    { Property Accessors }
    function Get_Type_: IXMLType_;
    function Get_Datatype: WideString;
    function Get_Domain_Icon: WideString;
    function Get_Physical_Domain_Name: IXMLPhysical_Domain_NameType;
    function Get_Parent_Domain: IXMLParent_DomainType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Datatype(Value: WideString);
    procedure Set_Domain_Icon(Value: WideString);
    { Methods & Properties }
    property Type_: IXMLType_ read Get_Type_;
    property Datatype: WideString read Get_Datatype write Set_Datatype;
    property Domain_Icon: WideString read Get_Domain_Icon write Set_Domain_Icon;
    property Physical_Domain_Name: IXMLPhysical_Domain_NameType read Get_Physical_Domain_Name;
    property Parent_Domain: IXMLParent_DomainType read Get_Parent_Domain;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
  end;

{ IXMLPhysical_Domain_NameType }

  IXMLPhysical_Domain_NameType = interface(IXMLNode)
    ['{C4D652DD-5A5E-442A-A768-A539E7A8D54C}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLParent_DomainType }

  IXMLParent_DomainType = interface(IXMLNode)
    ['{EE15A143-8B76-4422-85A0-796E20C11604}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLBitmap_GroupsType }

  IXMLBitmap_GroupsType = interface(IXMLNodeCollection)
    ['{D3CC42AE-8FB2-4F04-BDDC-955262D96635}']
    { Property Accessors }
    function Get_Bitmap(Index: Integer): IXMLBitmapType;
    { Methods & Properties }
    function Add: IXMLBitmapType;
    function Insert(const Index: Integer): IXMLBitmapType;
    property Bitmap[Index: Integer]: IXMLBitmapType read Get_Bitmap; default;
  end;

{ IXMLBitmapType }

  IXMLBitmapType = interface(IXMLNode)
    ['{25E1F45F-6F09-4B25-BF86-D9BB348312D6}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_BitmapProps: IXMLBitmapPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property BitmapProps: IXMLBitmapPropsType read Get_BitmapProps;
  end;

{ IXMLBitmapPropsType }

  IXMLBitmapPropsType = interface(IXMLNode)
    ['{97A19691-B736-48B5-953C-B35090153D9C}']
    { Property Accessors }
    function Get_Bitmap_Filename: WideString;
    function Get_Bitmap_DIB_Size: IXMLBitmap_DIB_SizeType;
    function Get_Bitmap_DIB_Handle: IXMLBitmap_DIB_HandleType;
    function Get_Flags: IXMLFlagsType;
    function Get_Image_Index: IXMLImage_IndexType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Bitmap_Filename(Value: WideString);
    { Methods & Properties }
    property Bitmap_Filename: WideString read Get_Bitmap_Filename write Set_Bitmap_Filename;
    property Bitmap_DIB_Size: IXMLBitmap_DIB_SizeType read Get_Bitmap_DIB_Size;
    property Bitmap_DIB_Handle: IXMLBitmap_DIB_HandleType read Get_Bitmap_DIB_Handle;
    property Flags: IXMLFlagsType read Get_Flags;
    property Image_Index: IXMLImage_IndexType read Get_Image_Index;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
  end;

{ IXMLBitmap_DIB_SizeType }

  IXMLBitmap_DIB_SizeType = interface(IXMLNode)
    ['{45B88D89-B082-4E1D-BBF0-840065CDE895}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLBitmap_DIB_HandleType }

  IXMLBitmap_DIB_HandleType = interface(IXMLNode)
    ['{50EC9F40-E6FB-496B-AB25-E1B00C22D363}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLFlagsType }

  IXMLFlagsType = interface(IXMLNode)
    ['{85E58B4F-AA41-4604-BFC4-995C94AC75A9}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLImage_IndexType }

  IXMLImage_IndexType = interface(IXMLNode)
    ['{A3B18837-C155-44DB-9DDC-0D2EBC9F5147}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLSubject_Area_GroupsType }

  IXMLSubject_Area_GroupsType = interface(IXMLNodeCollection)
    ['{390E3025-425D-49CF-BB0F-FB99AB06B0B1}']
    { Property Accessors }
    function Get_Subject_Area(Index: Integer): IXMLSubject_AreaType;
    { Methods & Properties }
    function Add: IXMLSubject_AreaType;
    function Insert(const Index: Integer): IXMLSubject_AreaType;
    property Subject_Area[Index: Integer]: IXMLSubject_AreaType read Get_Subject_Area; default;
  end;

{ IXMLSubject_AreaType }

  IXMLSubject_AreaType = interface(IXMLNode)
    ['{2A7C3824-3DAE-4D75-A08F-8FAFF1817F3F}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Subject_AreaProps: IXMLSubject_AreaPropsType;
    function Get_Stored_Display_Groups: IXMLStored_Display_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Subject_AreaProps: IXMLSubject_AreaPropsType read Get_Subject_AreaProps;
    property Stored_Display_Groups: IXMLStored_Display_GroupsType read Get_Stored_Display_Groups;
  end;

{ IXMLSubject_AreaPropsType }

  IXMLSubject_AreaPropsType = interface(IXMLNode)
    ['{EDF17E13-917B-4F5A-8F5A-C19A0CD92A23}']
    { Property Accessors }
    function Get_Name: IXMLNameType;
    function Get_Referenced_Entities: IXMLReferenced_EntitiesTypeList;
    function Get_Referenced_Relationships: IXMLReferenced_RelationshipsTypeList;
    function Get_Created_Time: Integer;
    function Get_Modified_Time: Integer;
    function Get_Is_Locked: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_Filter_Dangling_Rels_from_Schema_Gen: WideString;
    procedure Set_Created_Time(Value: Integer);
    procedure Set_Modified_Time(Value: Integer);
    procedure Set_Is_Locked(Value: WideString);
    procedure Set_Filter_Dangling_Rels_from_Schema_Gen(Value: WideString);
    { Methods & Properties }
    property Name: IXMLNameType read Get_Name;
    property Referenced_Entities: IXMLReferenced_EntitiesTypeList read Get_Referenced_Entities;
    property Referenced_Relationships: IXMLReferenced_RelationshipsTypeList read Get_Referenced_Relationships;
    property Created_Time: Integer read Get_Created_Time write Set_Created_Time;
    property Modified_Time: Integer read Get_Modified_Time write Set_Modified_Time;
    property Is_Locked: WideString read Get_Is_Locked write Set_Is_Locked;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
    property Filter_Dangling_Rels_from_Schema_Gen: WideString read Get_Filter_Dangling_Rels_from_Schema_Gen write Set_Filter_Dangling_Rels_from_Schema_Gen;
  end;

{ IXMLNameType }

  IXMLNameType = interface(IXMLNode)
    ['{7ADB6879-FF07-456D-92BA-5280E8549127}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLReferenced_EntitiesType }

  IXMLReferenced_EntitiesType = interface(IXMLNode)
    ['{87CE377D-CE0B-4AAF-B8AF-D4DD5CC025A8}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLReferenced_EntitiesTypeList }

  IXMLReferenced_EntitiesTypeList = interface(IXMLNodeCollection)
    ['{35056F0D-7583-4B27-954A-3A5118D8F0E0}']
    { Methods & Properties }
    function Add: IXMLReferenced_EntitiesType;
    function Insert(const Index: Integer): IXMLReferenced_EntitiesType;
    function Get_Item(Index: Integer): IXMLReferenced_EntitiesType;
    property Items[Index: Integer]: IXMLReferenced_EntitiesType read Get_Item; default;
  end;

{ IXMLReferenced_RelationshipsType }

  IXMLReferenced_RelationshipsType = interface(IXMLNode)
    ['{F9328D20-DAC1-42FF-8DBE-9D9C1CA86F07}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLReferenced_RelationshipsTypeList }

  IXMLReferenced_RelationshipsTypeList = interface(IXMLNodeCollection)
    ['{149B5456-5434-4111-95B8-823C599E481B}']
    { Methods & Properties }
    function Add: IXMLReferenced_RelationshipsType;
    function Insert(const Index: Integer): IXMLReferenced_RelationshipsType;
    function Get_Item(Index: Integer): IXMLReferenced_RelationshipsType;
    property Items[Index: Integer]: IXMLReferenced_RelationshipsType read Get_Item; default;
  end;

{ IXMLStored_Display_GroupsType }

  IXMLStored_Display_GroupsType = interface(IXMLNode)
    ['{8D1D561E-19F0-4113-AE25-4A3C591DF4C7}']
    { Property Accessors }
    function Get_Stored_Display: IXMLStored_DisplayType;
    { Methods & Properties }
    property Stored_Display: IXMLStored_DisplayType read Get_Stored_Display;
  end;

{ IXMLStored_DisplayType }

  IXMLStored_DisplayType = interface(IXMLNode)
    ['{569F9226-7048-4F12-A148-547FD6FC907F}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Stored_DisplayProps: IXMLStored_DisplayPropsType;
    function Get_Drawing_Object_Entity_Groups: IXMLDrawing_Object_Entity_GroupsType;
    function Get_Drawing_Object_Relationship_Groups: IXMLDrawing_Object_Relationship_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Stored_DisplayProps: IXMLStored_DisplayPropsType read Get_Stored_DisplayProps;
    property Drawing_Object_Entity_Groups: IXMLDrawing_Object_Entity_GroupsType read Get_Drawing_Object_Entity_Groups;
    property Drawing_Object_Relationship_Groups: IXMLDrawing_Object_Relationship_GroupsType read Get_Drawing_Object_Relationship_Groups;
  end;

{ IXMLStored_DisplayPropsType }

  IXMLStored_DisplayPropsType = interface(IXMLNode)
    ['{C0FB21F9-CB99-488E-A627-8C486B20444A}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Display_Rel_Name: Integer;
    function Get_Display_Cardinality: Integer;
    function Get_Display_Not_Null: Integer;
    function Get_Display_Physical_Level: Integer;
    function Get_Display_Datatype: Integer;
    function Get_Display_Rolenames: Integer;
    function Get_Display_Alt_Keys: Integer;
    function Get_Display_Level: Integer;
    function Get_Zoom_Option: Integer;
    function Get_Display_FK: Integer;
    function Get_Display_Subcat_Name: Integer;
    function Get_Display_Referantial_Integrity: Integer;
    function Get_Physical_Display_Level: Integer;
    function Get_Physical_Display_FK_Name: Integer;
    function Get_Physical_Display_Cardinality: Integer;
    function Get_Physical_Display_Ref_Integ: Integer;
    function Get_Physical_Display_Alt_Keys: Integer;
    function Get_Disallow_Manual_Rel_Layout: WideString;
    function Get_Dont_Display_Views: WideString;
    function Get_Dont_Display_View_Rels: WideString;
    function Get_Display_View_Col_Datatype: WideString;
    function Get_Display_View_Col_Null_Option: WideString;
    function Get_Display_Ungenerated: WideString;
    function Get_Show_Migrated_Atts: WideString;
    function Get_Display_Logical_Datatype: WideString;
    function Get_Display_Logical_FK_Deisgnator: WideString;
    function Get_Display_Diagonal_Lines: WideString;
    function Get_Display_Attribute_Icons: WideString;
    function Get_Display_Entity_Icons: WideString;
    function Get_Display_Logical_PK_Designator: WideString;
    function Get_Display_Physical_PK_Designator: WideString;
    function Get_Display_DW_Icons: WideString;
    function Get_Display_View_Col_Expr: WideString;
    function Get_Hide_Rels: WideString;
    function Get_Display_Danging_Rels: WideString;
    function Get_Disallow_Manual_Entity_Resize: WideString;
    function Get_Shadow: WideString;
    function Get_Shadow_X: Integer;
    function Get_Shadow_Y: Integer;
    function Get_V_Scroll_Pos: Integer;
    function Get_H_Scroll_Pos: Integer;
    function Get_Modified_Time: Integer;
    function Get_Is_Locked: WideString;
    function Get_Display_Logical_Domains: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_Print_Info: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Display_Rel_Name(Value: Integer);
    procedure Set_Display_Cardinality(Value: Integer);
    procedure Set_Display_Not_Null(Value: Integer);
    procedure Set_Display_Physical_Level(Value: Integer);
    procedure Set_Display_Datatype(Value: Integer);
    procedure Set_Display_Rolenames(Value: Integer);
    procedure Set_Display_Alt_Keys(Value: Integer);
    procedure Set_Display_Level(Value: Integer);
    procedure Set_Zoom_Option(Value: Integer);
    procedure Set_Display_FK(Value: Integer);
    procedure Set_Display_Subcat_Name(Value: Integer);
    procedure Set_Display_Referantial_Integrity(Value: Integer);
    procedure Set_Physical_Display_Level(Value: Integer);
    procedure Set_Physical_Display_FK_Name(Value: Integer);
    procedure Set_Physical_Display_Cardinality(Value: Integer);
    procedure Set_Physical_Display_Ref_Integ(Value: Integer);
    procedure Set_Physical_Display_Alt_Keys(Value: Integer);
    procedure Set_Disallow_Manual_Rel_Layout(Value: WideString);
    procedure Set_Dont_Display_Views(Value: WideString);
    procedure Set_Dont_Display_View_Rels(Value: WideString);
    procedure Set_Display_View_Col_Datatype(Value: WideString);
    procedure Set_Display_View_Col_Null_Option(Value: WideString);
    procedure Set_Display_Ungenerated(Value: WideString);
    procedure Set_Show_Migrated_Atts(Value: WideString);
    procedure Set_Display_Logical_Datatype(Value: WideString);
    procedure Set_Display_Logical_FK_Deisgnator(Value: WideString);
    procedure Set_Display_Diagonal_Lines(Value: WideString);
    procedure Set_Display_Attribute_Icons(Value: WideString);
    procedure Set_Display_Entity_Icons(Value: WideString);
    procedure Set_Display_Logical_PK_Designator(Value: WideString);
    procedure Set_Display_Physical_PK_Designator(Value: WideString);
    procedure Set_Display_DW_Icons(Value: WideString);
    procedure Set_Display_View_Col_Expr(Value: WideString);
    procedure Set_Hide_Rels(Value: WideString);
    procedure Set_Display_Danging_Rels(Value: WideString);
    procedure Set_Disallow_Manual_Entity_Resize(Value: WideString);
    procedure Set_Shadow(Value: WideString);
    procedure Set_Shadow_X(Value: Integer);
    procedure Set_Shadow_Y(Value: Integer);
    procedure Set_V_Scroll_Pos(Value: Integer);
    procedure Set_H_Scroll_Pos(Value: Integer);
    procedure Set_Modified_Time(Value: Integer);
    procedure Set_Is_Locked(Value: WideString);
    procedure Set_Display_Logical_Domains(Value: WideString);
    procedure Set_Print_Info(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Display_Rel_Name: Integer read Get_Display_Rel_Name write Set_Display_Rel_Name;
    property Display_Cardinality: Integer read Get_Display_Cardinality write Set_Display_Cardinality;
    property Display_Not_Null: Integer read Get_Display_Not_Null write Set_Display_Not_Null;
    property Display_Physical_Level: Integer read Get_Display_Physical_Level write Set_Display_Physical_Level;
    property Display_Datatype: Integer read Get_Display_Datatype write Set_Display_Datatype;
    property Display_Rolenames: Integer read Get_Display_Rolenames write Set_Display_Rolenames;
    property Display_Alt_Keys: Integer read Get_Display_Alt_Keys write Set_Display_Alt_Keys;
    property Display_Level: Integer read Get_Display_Level write Set_Display_Level;
    property Zoom_Option: Integer read Get_Zoom_Option write Set_Zoom_Option;
    property Display_FK: Integer read Get_Display_FK write Set_Display_FK;
    property Display_Subcat_Name: Integer read Get_Display_Subcat_Name write Set_Display_Subcat_Name;
    property Display_Referantial_Integrity: Integer read Get_Display_Referantial_Integrity write Set_Display_Referantial_Integrity;
    property Physical_Display_Level: Integer read Get_Physical_Display_Level write Set_Physical_Display_Level;
    property Physical_Display_FK_Name: Integer read Get_Physical_Display_FK_Name write Set_Physical_Display_FK_Name;
    property Physical_Display_Cardinality: Integer read Get_Physical_Display_Cardinality write Set_Physical_Display_Cardinality;
    property Physical_Display_Ref_Integ: Integer read Get_Physical_Display_Ref_Integ write Set_Physical_Display_Ref_Integ;
    property Physical_Display_Alt_Keys: Integer read Get_Physical_Display_Alt_Keys write Set_Physical_Display_Alt_Keys;
    property Disallow_Manual_Rel_Layout: WideString read Get_Disallow_Manual_Rel_Layout write Set_Disallow_Manual_Rel_Layout;
    property Dont_Display_Views: WideString read Get_Dont_Display_Views write Set_Dont_Display_Views;
    property Dont_Display_View_Rels: WideString read Get_Dont_Display_View_Rels write Set_Dont_Display_View_Rels;
    property Display_View_Col_Datatype: WideString read Get_Display_View_Col_Datatype write Set_Display_View_Col_Datatype;
    property Display_View_Col_Null_Option: WideString read Get_Display_View_Col_Null_Option write Set_Display_View_Col_Null_Option;
    property Display_Ungenerated: WideString read Get_Display_Ungenerated write Set_Display_Ungenerated;
    property Show_Migrated_Atts: WideString read Get_Show_Migrated_Atts write Set_Show_Migrated_Atts;
    property Display_Logical_Datatype: WideString read Get_Display_Logical_Datatype write Set_Display_Logical_Datatype;
    property Display_Logical_FK_Deisgnator: WideString read Get_Display_Logical_FK_Deisgnator write Set_Display_Logical_FK_Deisgnator;
    property Display_Diagonal_Lines: WideString read Get_Display_Diagonal_Lines write Set_Display_Diagonal_Lines;
    property Display_Attribute_Icons: WideString read Get_Display_Attribute_Icons write Set_Display_Attribute_Icons;
    property Display_Entity_Icons: WideString read Get_Display_Entity_Icons write Set_Display_Entity_Icons;
    property Display_Logical_PK_Designator: WideString read Get_Display_Logical_PK_Designator write Set_Display_Logical_PK_Designator;
    property Display_Physical_PK_Designator: WideString read Get_Display_Physical_PK_Designator write Set_Display_Physical_PK_Designator;
    property Display_DW_Icons: WideString read Get_Display_DW_Icons write Set_Display_DW_Icons;
    property Display_View_Col_Expr: WideString read Get_Display_View_Col_Expr write Set_Display_View_Col_Expr;
    property Hide_Rels: WideString read Get_Hide_Rels write Set_Hide_Rels;
    property Display_Danging_Rels: WideString read Get_Display_Danging_Rels write Set_Display_Danging_Rels;
    property Disallow_Manual_Entity_Resize: WideString read Get_Disallow_Manual_Entity_Resize write Set_Disallow_Manual_Entity_Resize;
    property Shadow: WideString read Get_Shadow write Set_Shadow;
    property Shadow_X: Integer read Get_Shadow_X write Set_Shadow_X;
    property Shadow_Y: Integer read Get_Shadow_Y write Set_Shadow_Y;
    property V_Scroll_Pos: Integer read Get_V_Scroll_Pos write Set_V_Scroll_Pos;
    property H_Scroll_Pos: Integer read Get_H_Scroll_Pos write Set_H_Scroll_Pos;
    property Modified_Time: Integer read Get_Modified_Time write Set_Modified_Time;
    property Is_Locked: WideString read Get_Is_Locked write Set_Is_Locked;
    property Display_Logical_Domains: WideString read Get_Display_Logical_Domains write Set_Display_Logical_Domains;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
    property Print_Info: WideString read Get_Print_Info write Set_Print_Info;
  end;

{ IXMLDrawing_Object_Entity_GroupsType }

  IXMLDrawing_Object_Entity_GroupsType = interface(IXMLNodeCollection)
    ['{68BB2554-D532-47D8-B284-27EEBFBA6800}']
    { Property Accessors }
    function Get_Drawing_Object_Entity(Index: Integer): IXMLDrawing_Object_EntityType;
    { Methods & Properties }
    function Add: IXMLDrawing_Object_EntityType;
    function Insert(const Index: Integer): IXMLDrawing_Object_EntityType;
    property Drawing_Object_Entity[Index: Integer]: IXMLDrawing_Object_EntityType read Get_Drawing_Object_Entity; default;
  end;

{ IXMLDrawing_Object_EntityType }

  IXMLDrawing_Object_EntityType = interface(IXMLNode)
    ['{E375BA15-2692-426E-8A91-32D17ACBB868}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Drawing_Object_EntityProps: IXMLDrawing_Object_EntityPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Drawing_Object_EntityProps: IXMLDrawing_Object_EntityPropsType read Get_Drawing_Object_EntityProps;
  end;

{ IXMLDrawing_Object_EntityPropsType }

  IXMLDrawing_Object_EntityPropsType = interface(IXMLNode)
    ['{2FDE3695-7233-4B2F-A16A-C92BDDE050A5}']
    { Property Accessors }
    function Get_DO_Text: WideString;
    function Get_DO_Location: WideString;
    function Get_DO_Reference_Object: WideString;
    function Get_DO_Entity_Width_AutoResizeable: WideString;
    function Get_DO_Entity_Height_AutoResizeable: WideString;
    procedure Set_DO_Text(Value: WideString);
    procedure Set_DO_Location(Value: WideString);
    procedure Set_DO_Reference_Object(Value: WideString);
    procedure Set_DO_Entity_Width_AutoResizeable(Value: WideString);
    procedure Set_DO_Entity_Height_AutoResizeable(Value: WideString);
    { Methods & Properties }
    property DO_Text: WideString read Get_DO_Text write Set_DO_Text;
    property DO_Location: WideString read Get_DO_Location write Set_DO_Location;
    property DO_Reference_Object: WideString read Get_DO_Reference_Object write Set_DO_Reference_Object;
    property DO_Entity_Width_AutoResizeable: WideString read Get_DO_Entity_Width_AutoResizeable write Set_DO_Entity_Width_AutoResizeable;
    property DO_Entity_Height_AutoResizeable: WideString read Get_DO_Entity_Height_AutoResizeable write Set_DO_Entity_Height_AutoResizeable;
  end;

{ IXMLDrawing_Object_Relationship_GroupsType }

  IXMLDrawing_Object_Relationship_GroupsType = interface(IXMLNode)
    ['{0D6BEB15-8FE6-4185-8462-484E35D8FB6A}']
    { Property Accessors }
    function Get_Drawing_Object_Relationship: IXMLDrawing_Object_RelationshipType;
    { Methods & Properties }
    property Drawing_Object_Relationship: IXMLDrawing_Object_RelationshipType read Get_Drawing_Object_Relationship;
  end;

{ IXMLDrawing_Object_RelationshipType }

  IXMLDrawing_Object_RelationshipType = interface(IXMLNode)
    ['{099A15E9-6AFA-4FE8-8A31-BECAAF52B58F}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Drawing_Object_RelationshipProps: IXMLDrawing_Object_RelationshipPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Drawing_Object_RelationshipProps: IXMLDrawing_Object_RelationshipPropsType read Get_Drawing_Object_RelationshipProps;
  end;

{ IXMLDrawing_Object_RelationshipPropsType }

  IXMLDrawing_Object_RelationshipPropsType = interface(IXMLNode)
    ['{FEA578A3-964B-4689-9FA3-DCA9A36F62E8}']
    { Property Accessors }
    function Get_DO_Text: WideString;
    function Get_DO_Reference_Object: WideString;
    function Get_DO_Relationship_Path: IXMLString_List;
    function Get_DO_User_Controled_Path: WideString;
    function Get_DO_Parent_Side_Verb_Phrase: Integer;
    function Get_DO_Childside_Verb_Phrase: Integer;
    function Get_DO_Mid_Point_of_Relationship: WideString;
    function Get_DO_Parent_Side_Verb_Phrase_Coordinates: WideString;
    function Get_DO_Child_Side_Verb_Phrase_Coordinates: WideString;
    function Get_DO_Color_Inherited: WideString;
    function Get_DO_Font_Inherited: WideString;
    procedure Set_DO_Text(Value: WideString);
    procedure Set_DO_Reference_Object(Value: WideString);
    procedure Set_DO_User_Controled_Path(Value: WideString);
    procedure Set_DO_Parent_Side_Verb_Phrase(Value: Integer);
    procedure Set_DO_Childside_Verb_Phrase(Value: Integer);
    procedure Set_DO_Mid_Point_of_Relationship(Value: WideString);
    procedure Set_DO_Parent_Side_Verb_Phrase_Coordinates(Value: WideString);
    procedure Set_DO_Child_Side_Verb_Phrase_Coordinates(Value: WideString);
    procedure Set_DO_Color_Inherited(Value: WideString);
    procedure Set_DO_Font_Inherited(Value: WideString);
    { Methods & Properties }
    property DO_Text: WideString read Get_DO_Text write Set_DO_Text;
    property DO_Reference_Object: WideString read Get_DO_Reference_Object write Set_DO_Reference_Object;
    property DO_Relationship_Path: IXMLString_List read Get_DO_Relationship_Path;
    property DO_User_Controled_Path: WideString read Get_DO_User_Controled_Path write Set_DO_User_Controled_Path;
    property DO_Parent_Side_Verb_Phrase: Integer read Get_DO_Parent_Side_Verb_Phrase write Set_DO_Parent_Side_Verb_Phrase;
    property DO_Childside_Verb_Phrase: Integer read Get_DO_Childside_Verb_Phrase write Set_DO_Childside_Verb_Phrase;
    property DO_Mid_Point_of_Relationship: WideString read Get_DO_Mid_Point_of_Relationship write Set_DO_Mid_Point_of_Relationship;
    property DO_Parent_Side_Verb_Phrase_Coordinates: WideString read Get_DO_Parent_Side_Verb_Phrase_Coordinates write Set_DO_Parent_Side_Verb_Phrase_Coordinates;
    property DO_Child_Side_Verb_Phrase_Coordinates: WideString read Get_DO_Child_Side_Verb_Phrase_Coordinates write Set_DO_Child_Side_Verb_Phrase_Coordinates;
    property DO_Color_Inherited: WideString read Get_DO_Color_Inherited write Set_DO_Color_Inherited;
    property DO_Font_Inherited: WideString read Get_DO_Font_Inherited write Set_DO_Font_Inherited;
  end;

{ IXMLRelationship_GroupsType }

  IXMLRelationship_GroupsType = interface(IXMLNodeCollection)
    ['{F9700A44-2FA4-4F8A-B61C-45D73161FD89}']
    { Property Accessors }
    function Get_Relationship(Index: Integer): IXMLRelationshipType;
    { Methods & Properties }
    function Add: IXMLRelationshipType;
    function Insert(const Index: Integer): IXMLRelationshipType;
    property Relationship[Index: Integer]: IXMLRelationshipType read Get_Relationship; default;
  end;

{ IXMLRelationshipType }

  IXMLRelationshipType = interface(IXMLNode)
    ['{E739CEC2-DEB6-441B-96E3-DC0BDC402D8F}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_RelationshipProps: IXMLRelationshipPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property RelationshipProps: IXMLRelationshipPropsType read Get_RelationshipProps;
  end;

{ IXMLRelationshipPropsType }

  IXMLRelationshipPropsType = interface(IXMLNode)
    ['{6BE142E9-9D46-4070-B961-BDAD8D43FDAC}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Type_: Integer;
    function Get_Note: WideString;
    function Get_Cardinality: WideString;
    function Get_Relationship_No_Nulls: Integer;
    function Get_Relationship_Sequence: Integer;
    function Get_Relationship_Parent_Update_Rule: Integer;
    function Get_Relationship_Parent_Delete_Rule: Integer;
    function Get_Relationship_Parent_Insert_Rule: Integer;
    function Get_Relationship_Child_Update_Rule: Integer;
    function Get_Relationship_Child_Delete_Rule: Integer;
    function Get_Relationship_Child_Insert_Rule: Integer;
    function Get_Physical_Only: WideString;
    function Get_Relationship_Parent_Entity: WideString;
    function Get_Relationship_Child_Entity: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Type_(Value: Integer);
    procedure Set_Note(Value: WideString);
    procedure Set_Cardinality(Value: WideString);
    procedure Set_Relationship_No_Nulls(Value: Integer);
    procedure Set_Relationship_Sequence(Value: Integer);
    procedure Set_Relationship_Parent_Update_Rule(Value: Integer);
    procedure Set_Relationship_Parent_Delete_Rule(Value: Integer);
    procedure Set_Relationship_Parent_Insert_Rule(Value: Integer);
    procedure Set_Relationship_Child_Update_Rule(Value: Integer);
    procedure Set_Relationship_Child_Delete_Rule(Value: Integer);
    procedure Set_Relationship_Child_Insert_Rule(Value: Integer);
    procedure Set_Physical_Only(Value: WideString);
    procedure Set_Relationship_Parent_Entity(Value: WideString);
    procedure Set_Relationship_Child_Entity(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Type_: Integer read Get_Type_ write Set_Type_;
    property Note: WideString read Get_Note write Set_Note;
    property Cardinality: WideString read Get_Cardinality write Set_Cardinality;
    property Relationship_No_Nulls: Integer read Get_Relationship_No_Nulls write Set_Relationship_No_Nulls;
    property Relationship_Sequence: Integer read Get_Relationship_Sequence write Set_Relationship_Sequence;
    property Relationship_Parent_Update_Rule: Integer read Get_Relationship_Parent_Update_Rule write Set_Relationship_Parent_Update_Rule;
    property Relationship_Parent_Delete_Rule: Integer read Get_Relationship_Parent_Delete_Rule write Set_Relationship_Parent_Delete_Rule;
    property Relationship_Parent_Insert_Rule: Integer read Get_Relationship_Parent_Insert_Rule write Set_Relationship_Parent_Insert_Rule;
    property Relationship_Child_Update_Rule: Integer read Get_Relationship_Child_Update_Rule write Set_Relationship_Child_Update_Rule;
    property Relationship_Child_Delete_Rule: Integer read Get_Relationship_Child_Delete_Rule write Set_Relationship_Child_Delete_Rule;
    property Relationship_Child_Insert_Rule: Integer read Get_Relationship_Child_Insert_Rule write Set_Relationship_Child_Insert_Rule;
    property Physical_Only: WideString read Get_Physical_Only write Set_Physical_Only;
    property Relationship_Parent_Entity: WideString read Get_Relationship_Parent_Entity write Set_Relationship_Parent_Entity;
    property Relationship_Child_Entity: WideString read Get_Relationship_Child_Entity write Set_Relationship_Child_Entity;
  end;

{ IXMLDefault_Value_GroupsType }

  IXMLDefault_Value_GroupsType = interface(IXMLNodeCollection)
    ['{50D88E04-D389-464D-8652-3B8D398B44EA}']
    { Property Accessors }
    function Get_Default_Value(Index: Integer): IXMLDefault_ValueType;
    { Methods & Properties }
    function Add: IXMLDefault_ValueType;
    function Insert(const Index: Integer): IXMLDefault_ValueType;
    property Default_Value[Index: Integer]: IXMLDefault_ValueType read Get_Default_Value; default;
  end;

{ IXMLDefault_ValueType }

  IXMLDefault_ValueType = interface(IXMLNode)
    ['{56E3E210-F4DE-40C8-ACA4-29D964A37C28}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Default_ValueProps: IXMLDefault_ValuePropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Default_ValueProps: IXMLDefault_ValuePropsType read Get_Default_ValueProps;
  end;

{ IXMLDefault_ValuePropsType }

  IXMLDefault_ValuePropsType = interface(IXMLNode)
    ['{B8F33693-2CEC-4BAF-8678-A4407F73F33C}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Usage_Count: Integer;
    function Get_Server_Value: WideString;
    function Get_Physical_Only: WideString;
    function Get_Physical_Name: WideString;
    function Get_LogicalDefault_Value: WideString;
    function Get_Is_Builtin: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_System_Generated: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Usage_Count(Value: Integer);
    procedure Set_Server_Value(Value: WideString);
    procedure Set_Physical_Only(Value: WideString);
    procedure Set_Physical_Name(Value: WideString);
    procedure Set_LogicalDefault_Value(Value: WideString);
    procedure Set_Is_Builtin(Value: WideString);
    procedure Set_System_Generated(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Usage_Count: Integer read Get_Usage_Count write Set_Usage_Count;
    property Server_Value: WideString read Get_Server_Value write Set_Server_Value;
    property Physical_Only: WideString read Get_Physical_Only write Set_Physical_Only;
    property Physical_Name: WideString read Get_Physical_Name write Set_Physical_Name;
    property LogicalDefault_Value: WideString read Get_LogicalDefault_Value write Set_LogicalDefault_Value;
    property Is_Builtin: WideString read Get_Is_Builtin write Set_Is_Builtin;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
    property System_Generated: WideString read Get_System_Generated write Set_System_Generated;
  end;

{ IXMLTrigger_Template_GroupsType }

  IXMLTrigger_Template_GroupsType = interface(IXMLNodeCollection)
    ['{2292BD6A-90B8-4E8C-81E1-FE7527DA8975}']
    { Property Accessors }
    function Get_Trigger_Template(Index: Integer): IXMLTrigger_TemplateType;
    { Methods & Properties }
    function Add: IXMLTrigger_TemplateType;
    function Insert(const Index: Integer): IXMLTrigger_TemplateType;
    property Trigger_Template[Index: Integer]: IXMLTrigger_TemplateType read Get_Trigger_Template; default;
  end;

{ IXMLTrigger_TemplateType }

  IXMLTrigger_TemplateType = interface(IXMLNode)
    ['{75980037-E5D6-4AB7-91E2-4AD4EA9A7FA8}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Trigger_TemplateProps: IXMLTrigger_TemplatePropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Trigger_TemplateProps: IXMLTrigger_TemplatePropsType read Get_Trigger_TemplateProps;
  end;

{ IXMLTrigger_TemplatePropsType }

  IXMLTrigger_TemplatePropsType = interface(IXMLNode)
    ['{29E2B834-A9EE-47AE-833F-C2A75BC8D62D}']
    { Property Accessors }
    function Get_Name: IXMLNameType;
    function Get_Type_: IXMLType_;
    function Get_Template_Code: IXMLTemplate_CodeType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    { Methods & Properties }
    property Name: IXMLNameType read Get_Name;
    property Type_: IXMLType_ read Get_Type_;
    property Template_Code: IXMLTemplate_CodeType read Get_Template_Code;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
  end;

{ IXMLTemplate_CodeType }

  IXMLTemplate_CodeType = interface(IXMLNode)
    ['{141E91E7-24B7-4D3B-9CEA-8B08E25C6EF2}']
    { Property Accessors }
    function Get_Space: WideString;
    procedure Set_Space(Value: WideString);
    { Methods & Properties }
    property Space: WideString read Get_Space write Set_Space;
  end;

{ IXMLDefault_Trigger_Template_GroupsType }

  IXMLDefault_Trigger_Template_GroupsType = interface(IXMLNodeCollection)
    ['{72C725CE-CB37-47EE-8E7D-D7B3B77BAA34}']
    { Property Accessors }
    function Get_Default_Trigger_Template(Index: Integer): IXMLDefault_Trigger_TemplateType;
    { Methods & Properties }
    function Add: IXMLDefault_Trigger_TemplateType;
    function Insert(const Index: Integer): IXMLDefault_Trigger_TemplateType;
    property Default_Trigger_Template[Index: Integer]: IXMLDefault_Trigger_TemplateType read Get_Default_Trigger_Template; default;
  end;

{ IXMLDefault_Trigger_TemplateType }

  IXMLDefault_Trigger_TemplateType = interface(IXMLNode)
    ['{074DF37D-496C-4186-9B44-DD17877AD905}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Default_Trigger_TemplateProps: IXMLDefault_Trigger_TemplatePropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Default_Trigger_TemplateProps: IXMLDefault_Trigger_TemplatePropsType read Get_Default_Trigger_TemplateProps;
  end;

{ IXMLDefault_Trigger_TemplatePropsType }

  IXMLDefault_Trigger_TemplatePropsType = interface(IXMLNode)
    ['{5196063F-DB7E-4DB5-82E3-CB2EFF312DF0}']
    { Property Accessors }
    function Get_Trigger_Template_Ref: WideString;
    function Get_Template_Purpose_Text: IXMLTemplate_Purpose_TextType;
    function Get_Template_Purpose: IXMLTemplate_PurposeType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Trigger_Template_Ref(Value: WideString);
    { Methods & Properties }
    property Trigger_Template_Ref: WideString read Get_Trigger_Template_Ref write Set_Trigger_Template_Ref;
    property Template_Purpose_Text: IXMLTemplate_Purpose_TextType read Get_Template_Purpose_Text;
    property Template_Purpose: IXMLTemplate_PurposeType read Get_Template_Purpose;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
  end;

{ IXMLTemplate_Purpose_TextType }

  IXMLTemplate_Purpose_TextType = interface(IXMLNode)
    ['{10421A3D-6F7E-495E-B98E-EBD253D1EB43}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLTemplate_PurposeType }

  IXMLTemplate_PurposeType = interface(IXMLNode)
    ['{5707D8C2-44CE-44E2-AD92-4740A10DBC53}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLName_Mapping_GroupsType }

  IXMLName_Mapping_GroupsType = interface(IXMLNodeCollection)
    ['{887D78A5-8675-46F0-97AE-DA912FCAAC08}']
    { Property Accessors }
    function Get_Name_Mapping(Index: Integer): IXMLName_MappingType;
    { Methods & Properties }
    function Add: IXMLName_MappingType;
    function Insert(const Index: Integer): IXMLName_MappingType;
    property Name_Mapping[Index: Integer]: IXMLName_MappingType read Get_Name_Mapping; default;
  end;

{ IXMLName_MappingType }

  IXMLName_MappingType = interface(IXMLNode)
    ['{A4352EDC-12E8-413C-BE2A-71EC8FC93A1B}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Name_MappingProps: IXMLName_MappingPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Name_MappingProps: IXMLName_MappingPropsType read Get_Name_MappingProps;
  end;

{ IXMLName_MappingPropsType }

  IXMLName_MappingPropsType = interface(IXMLNode)
    ['{510709B7-9520-4973-8B91-1AE0FA17189A}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Name_Mapping_Obj_Type: Integer;
    function Get_Name_Mapping_Use_Glossary: WideString;
    function Get_Name_Mapping_Alternate_Abbrev: WideString;
    function Get_Name_Mapping_Macro_Text: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Name(Value: WideString);
    procedure Set_Name_Mapping_Obj_Type(Value: Integer);
    procedure Set_Name_Mapping_Use_Glossary(Value: WideString);
    procedure Set_Name_Mapping_Alternate_Abbrev(Value: WideString);
    procedure Set_Name_Mapping_Macro_Text(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Name_Mapping_Obj_Type: Integer read Get_Name_Mapping_Obj_Type write Set_Name_Mapping_Obj_Type;
    property Name_Mapping_Use_Glossary: WideString read Get_Name_Mapping_Use_Glossary write Set_Name_Mapping_Use_Glossary;
    property Name_Mapping_Alternate_Abbrev: WideString read Get_Name_Mapping_Alternate_Abbrev write Set_Name_Mapping_Alternate_Abbrev;
    property Name_Mapping_Macro_Text: WideString read Get_Name_Mapping_Macro_Text write Set_Name_Mapping_Macro_Text;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
  end;

{ IXMLNaming_Options_GroupsType }

  IXMLNaming_Options_GroupsType = interface(IXMLNodeCollection)
    ['{930F4CBF-4DFC-4248-AAA1-3526B1F3FCC3}']
    { Property Accessors }
    function Get_Naming_Options(Index: Integer): IXMLNaming_OptionsType;
    { Methods & Properties }
    function Add: IXMLNaming_OptionsType;
    function Insert(const Index: Integer): IXMLNaming_OptionsType;
    property Naming_Options[Index: Integer]: IXMLNaming_OptionsType read Get_Naming_Options; default;
  end;

{ IXMLNaming_OptionsType }

  IXMLNaming_OptionsType = interface(IXMLNode)
    ['{B0D08CBF-31FB-43BF-8303-4ED3149A775A}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Naming_OptionsProps: IXMLNaming_OptionsPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Name: WideString read Get_Name write Set_Name;
    property Naming_OptionsProps: IXMLNaming_OptionsPropsType read Get_Naming_OptionsProps;
  end;

{ IXMLNaming_OptionsPropsType }

  IXMLNaming_OptionsPropsType = interface(IXMLNode)
    ['{7EDE483F-F22B-4DD1-BA68-4704BCF3C87A}']
    { Property Accessors }
    function Get_Name: IXMLNameType;
    function Get_Physical_Only: IXMLPhysical_OnlyType;
    function Get_Name_Mapping_Obj_Type: Integer;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_Logical_Only: IXMLLogical_OnlyType;
    procedure Set_Name_Mapping_Obj_Type(Value: Integer);
    { Methods & Properties }
    property Name: IXMLNameType read Get_Name;
    property Physical_Only: IXMLPhysical_OnlyType read Get_Physical_Only;
    property Name_Mapping_Obj_Type: Integer read Get_Name_Mapping_Obj_Type write Set_Name_Mapping_Obj_Type;
    property Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType read Get_Default_Object_Alternative_Id;
    property Logical_Only: IXMLLogical_OnlyType read Get_Logical_Only;
  end;

{ IXMLPhysical_OnlyType }

  IXMLPhysical_OnlyType = interface(IXMLNode)
    ['{8DF2E4B4-532D-41E5-B659-E0E451FB9C4A}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLLogical_OnlyType }

  IXMLLogical_OnlyType = interface(IXMLNode)
    ['{896AE2B2-9BA9-4381-99D8-7048F66DE666}']
    { Property Accessors }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
    { Methods & Properties }
    property RO: WideString read Get_RO write Set_RO;
  end;

{ IXMLString_List }

  IXMLString_List = interface(IXMLNodeCollection)
    ['{BE41ABD9-AEA6-46DE-8156-188A1385E5B5}']
    { Methods & Properties }
    function Add(const Value: WideString): IXMLNode;
    function Insert(const Index: Integer; const Value: WideString): IXMLNode;
    function Get_Item(Index: Integer): WideString;
    property Items[Index: Integer]: WideString read Get_Item; default;
  end;

{ Forward Decls }

  TXMLERwin4Type = class;
  TXMLModelType = class;
  TXMLModelPropsType = class;
  TXMLType_ = class;
  TXMLFile_NameType = class;
  TXMLFile_FormatType = class;
  TXMLTarget_ServerType = class;
  TXMLDBMS_VersionType = class;
  TXMLDBMS_Minor_VersionType = class;
  TXMLEntity_GroupsType = class;
  TXMLEntityType = class;
  TXMLEntityPropsType = class;
  TXMLAttribute_GroupsType = class;
  TXMLAttributeType = class;
  TXMLAttributePropsType = class;
  TXMLMaster_AttributeType = class;
  TXMLHistory_Information_GroupsType = class;
  TXMLHistory_InformationType = class;
  TXMLHistory_InformationPropsType = class;
  TXMLDefault_Object_Alternative_IdType = class;
  TXMLKey_Group_GroupsType = class;
  TXMLKey_GroupType = class;
  TXMLKey_GroupPropsType = class;
  TXMLKey_Group_Member_GroupsType = class;
  TXMLKey_Group_MemberType = class;
  TXMLKey_Group_MemberPropsType = class;
  TXMLDomain_GroupsType = class;
  TXMLDomainType = class;
  TXMLDomainPropsType = class;
  TXMLPhysical_Domain_NameType = class;
  TXMLParent_DomainType = class;
  TXMLBitmap_GroupsType = class;
  TXMLBitmapType = class;
  TXMLBitmapPropsType = class;
  TXMLBitmap_DIB_SizeType = class;
  TXMLBitmap_DIB_HandleType = class;
  TXMLFlagsType = class;
  TXMLImage_IndexType = class;
  TXMLSubject_Area_GroupsType = class;
  TXMLSubject_AreaType = class;
  TXMLSubject_AreaPropsType = class;
  TXMLNameType = class;
  TXMLReferenced_EntitiesType = class;
  TXMLReferenced_EntitiesTypeList = class;
  TXMLReferenced_RelationshipsType = class;
  TXMLReferenced_RelationshipsTypeList = class;
  TXMLStored_Display_GroupsType = class;
  TXMLStored_DisplayType = class;
  TXMLStored_DisplayPropsType = class;
  TXMLDrawing_Object_Entity_GroupsType = class;
  TXMLDrawing_Object_EntityType = class;
  TXMLDrawing_Object_EntityPropsType = class;
  TXMLDrawing_Object_Relationship_GroupsType = class;
  TXMLDrawing_Object_RelationshipType = class;
  TXMLDrawing_Object_RelationshipPropsType = class;
  TXMLRelationship_GroupsType = class;
  TXMLRelationshipType = class;
  TXMLRelationshipPropsType = class;
  TXMLDefault_Value_GroupsType = class;
  TXMLDefault_ValueType = class;
  TXMLDefault_ValuePropsType = class;
  TXMLTrigger_Template_GroupsType = class;
  TXMLTrigger_TemplateType = class;
  TXMLTrigger_TemplatePropsType = class;
  TXMLTemplate_CodeType = class;
  TXMLDefault_Trigger_Template_GroupsType = class;
  TXMLDefault_Trigger_TemplateType = class;
  TXMLDefault_Trigger_TemplatePropsType = class;
  TXMLTemplate_Purpose_TextType = class;
  TXMLTemplate_PurposeType = class;
  TXMLName_Mapping_GroupsType = class;
  TXMLName_MappingType = class;
  TXMLName_MappingPropsType = class;
  TXMLNaming_Options_GroupsType = class;
  TXMLNaming_OptionsType = class;
  TXMLNaming_OptionsPropsType = class;
  TXMLPhysical_OnlyType = class;
  TXMLLogical_OnlyType = class;
  TXMLString_List = class;

{ TXMLERwin4Type }

  TXMLERwin4Type = class(TXMLNode, IXMLERwin4Type)
  protected
    { IXMLERwin4Type }
    function Get_FileVersion: Integer;
    function Get_Model: IXMLModelType;
    procedure Set_FileVersion(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLModelType }

  TXMLModelType = class(TXMLNode, IXMLModelType)
  protected
    { IXMLModelType }
    function Get_Id: WideString;
    function Get_ModelType: Integer;
    function Get_TargetServer: Integer;
    function Get_DBMSVersion: Integer;
    function Get_DBMSMinorVersion: Integer;
    function Get_ModelProps: IXMLModelPropsType;
    function Get_Entity_Groups: IXMLEntity_GroupsType;
    function Get_Domain_Groups: IXMLDomain_GroupsType;
    function Get_Bitmap_Groups: IXMLBitmap_GroupsType;
    function Get_Subject_Area_Groups: IXMLSubject_Area_GroupsType;
    function Get_Relationship_Groups: IXMLRelationship_GroupsType;
    function Get_Default_Value_Groups: IXMLDefault_Value_GroupsType;
    function Get_Trigger_Template_Groups: IXMLTrigger_Template_GroupsType;
    function Get_Default_Trigger_Template_Groups: IXMLDefault_Trigger_Template_GroupsType;
    function Get_Name_Mapping_Groups: IXMLName_Mapping_GroupsType;
    function Get_Naming_Options_Groups: IXMLNaming_Options_GroupsType;
    function Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_ModelType(Value: Integer);
    procedure Set_TargetServer(Value: Integer);
    procedure Set_DBMSVersion(Value: Integer);
    procedure Set_DBMSMinorVersion(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLModelPropsType }

  TXMLModelPropsType = class(TXMLNode, IXMLModelPropsType)
  protected
    { IXMLModelPropsType }
    function Get_Name: WideString;
    function Get_Type_: IXMLType_;
    function Get_Tracking_History_Objects: WideString;
    function Get_Tracking_History_Events: WideString;
    function Get_File_Name: IXMLFile_NameType;
    function Get_Page_Grid: Integer;
    function Get_File_Format: IXMLFile_FormatType;
    function Get_Entity_Width: Integer;
    function Get_Entity_Height: Integer;
    function Get_Layout_Grid: Integer;
    function Get_Layout_Grid_X: Integer;
    function Get_Layout_Grid_Y: Integer;
    function Get_Font_Height: Integer;
    function Get_Font_Width: Integer;
    function Get_Unique_Names: Integer;
    function Get_Target_Server: IXMLTarget_ServerType;
    function Get_Default_Datatype: WideString;
    function Get_Non_Key_Null: Integer;
    function Get_Default_Fonts_and_Colors: WideString;
    function Get_Repos: Integer;
    function Get_DBMS_Version: IXMLDBMS_VersionType;
    function Get_Logical_Notation: Integer;
    function Get_DBMS_Minor_Version: IXMLDBMS_Minor_VersionType;
    function Get_Old_Repos: Integer;
    function Get_Max_View_Expr_Display_Len: Integer;
    function Get_Physical_Notation: Integer;
    function Get_Index_Name_Macro: WideString;
    function Get_Max_Def_Display_Len: Integer;
    function Get_Table_Name_Macro: WideString;
    function Get_Current_Tool: WideString;
    function Get_MM_Preview: WideString;
    function Get_Saved_From_Previous_Version: WideString;
    function Get_WYSIWYG_Print: WideString;
    function Get_Model_Background_Color: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Tracking_History_Objects(Value: WideString);
    procedure Set_Tracking_History_Events(Value: WideString);
    procedure Set_Page_Grid(Value: Integer);
    procedure Set_Entity_Width(Value: Integer);
    procedure Set_Entity_Height(Value: Integer);
    procedure Set_Layout_Grid(Value: Integer);
    procedure Set_Layout_Grid_X(Value: Integer);
    procedure Set_Layout_Grid_Y(Value: Integer);
    procedure Set_Font_Height(Value: Integer);
    procedure Set_Font_Width(Value: Integer);
    procedure Set_Unique_Names(Value: Integer);
    procedure Set_Default_Datatype(Value: WideString);
    procedure Set_Non_Key_Null(Value: Integer);
    procedure Set_Default_Fonts_and_Colors(Value: WideString);
    procedure Set_Repos(Value: Integer);
    procedure Set_Logical_Notation(Value: Integer);
    procedure Set_Old_Repos(Value: Integer);
    procedure Set_Max_View_Expr_Display_Len(Value: Integer);
    procedure Set_Physical_Notation(Value: Integer);
    procedure Set_Index_Name_Macro(Value: WideString);
    procedure Set_Max_Def_Display_Len(Value: Integer);
    procedure Set_Table_Name_Macro(Value: WideString);
    procedure Set_Current_Tool(Value: WideString);
    procedure Set_MM_Preview(Value: WideString);
    procedure Set_Saved_From_Previous_Version(Value: WideString);
    procedure Set_WYSIWYG_Print(Value: WideString);
    procedure Set_Model_Background_Color(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLType_ }

  TXMLType_ = class(TXMLNode, IXMLType_)
  protected
    { IXMLType_ }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLFile_NameType }

  TXMLFile_NameType = class(TXMLNode, IXMLFile_NameType)
  protected
    { IXMLFile_NameType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLFile_FormatType }

  TXMLFile_FormatType = class(TXMLNode, IXMLFile_FormatType)
  protected
    { IXMLFile_FormatType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLTarget_ServerType }

  TXMLTarget_ServerType = class(TXMLNode, IXMLTarget_ServerType)
  protected
    { IXMLTarget_ServerType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLDBMS_VersionType }

  TXMLDBMS_VersionType = class(TXMLNode, IXMLDBMS_VersionType)
  protected
    { IXMLDBMS_VersionType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLDBMS_Minor_VersionType }

  TXMLDBMS_Minor_VersionType = class(TXMLNode, IXMLDBMS_Minor_VersionType)
  protected
    { IXMLDBMS_Minor_VersionType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLEntity_GroupsType }

  TXMLEntity_GroupsType = class(TXMLNodeCollection, IXMLEntity_GroupsType)
  protected
    { IXMLEntity_GroupsType }
    function Get_Entity(Index: Integer): IXMLEntityType;
    function Add: IXMLEntityType;
    function Insert(const Index: Integer): IXMLEntityType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLEntityType }

  TXMLEntityType = class(TXMLNode, IXMLEntityType)
  protected
    { IXMLEntityType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_EntityProps: IXMLEntityPropsType;
    function Get_Attribute_Groups: IXMLAttribute_GroupsType;
    function Get_Key_Group_Groups: IXMLKey_Group_GroupsType;
    function Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLEntityPropsType }

  TXMLEntityPropsType = class(TXMLNode, IXMLEntityPropsType)
  protected
    { IXMLEntityPropsType }
    function Get_Type_: Integer;
    function Get_Index_Generate: Integer;
    function Get_Physical_Name: WideString;
    function Get_Comment: WideString;
    function Get_Physical_Only: WideString;
    procedure Set_Type_(Value: Integer);
    procedure Set_Index_Generate(Value: Integer);
    procedure Set_Physical_Name(Value: WideString);
    procedure Set_Comment(Value: WideString);
    procedure Set_Physical_Only(Value: WideString);
  end;

{ TXMLAttribute_GroupsType }

  TXMLAttribute_GroupsType = class(TXMLNodeCollection, IXMLAttribute_GroupsType)
  protected
    { IXMLAttribute_GroupsType }
    function Get_Attribute(Index: Integer): IXMLAttributeType;
    function Add: IXMLAttributeType;
    function Insert(const Index: Integer): IXMLAttributeType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAttributeType }

  TXMLAttributeType = class(TXMLNode, IXMLAttributeType)
  protected
    { IXMLAttributeType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_AttributeProps: IXMLAttributePropsType;
    function Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAttributePropsType }

  TXMLAttributePropsType = class(TXMLNode, IXMLAttributePropsType)
  protected
    { IXMLAttributePropsType }
    function Get_Type_: Integer;
    function Get_Null_Option: Integer;
    function Get_Order: Integer;
    function Get_Physical_Order: Integer;
    function Get_Parent_Attribute: WideString;
    function Get_Parent_Relationship: WideString;
    function Get_Physical_Name: WideString;
    function Get_Parent_Domain: WideString;
    function Get_Hide_in_Logical: WideString;
    function Get_Hide_in_Physical: WideString;
    function Get_Master_Attribute: IXMLMaster_AttributeType;
    function Get_DO_Color_Inherited: WideString;
    function Get_DO_Font_Inherited: WideString;
    function Get_Datatype: WideString;
    function Get_Comment: WideString;
    function Get_Default: WideString;
    procedure Set_Type_(Value: Integer);
    procedure Set_Null_Option(Value: Integer);
    procedure Set_Order(Value: Integer);
    procedure Set_Physical_Order(Value: Integer);
    procedure Set_Parent_Attribute(Value: WideString);
    procedure Set_Parent_Relationship(Value: WideString);
    procedure Set_Physical_Name(Value: WideString);
    procedure Set_Parent_Domain(Value: WideString);
    procedure Set_Hide_in_Logical(Value: WideString);
    procedure Set_Hide_in_Physical(Value: WideString);
    procedure Set_DO_Color_Inherited(Value: WideString);
    procedure Set_DO_Font_Inherited(Value: WideString);
    procedure Set_Datatype(Value: WideString);
    procedure Set_Comment(Value: WideString);
    procedure Set_Default(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMaster_AttributeType }

  TXMLMaster_AttributeType = class(TXMLNode, IXMLMaster_AttributeType)
  protected
    { IXMLMaster_AttributeType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLHistory_Information_GroupsType }

  TXMLHistory_Information_GroupsType = class(TXMLNode, IXMLHistory_Information_GroupsType)
  protected
    { IXMLHistory_Information_GroupsType }
    function Get_History_Information: IXMLHistory_InformationType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLHistory_InformationType }

  TXMLHistory_InformationType = class(TXMLNode, IXMLHistory_InformationType)
  protected
    { IXMLHistory_InformationType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_History_InformationProps: IXMLHistory_InformationPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLHistory_InformationPropsType }

  TXMLHistory_InformationPropsType = class(TXMLNode, IXMLHistory_InformationPropsType)
  protected
    { IXMLHistory_InformationPropsType }
    function Get_Type_: Integer;
    function Get_Created_Time: Integer;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Type_(Value: Integer);
    procedure Set_Created_Time(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDefault_Object_Alternative_IdType }

  TXMLDefault_Object_Alternative_IdType = class(TXMLNode, IXMLDefault_Object_Alternative_IdType)
  protected
    { IXMLDefault_Object_Alternative_IdType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLKey_Group_GroupsType }

  TXMLKey_Group_GroupsType = class(TXMLNodeCollection, IXMLKey_Group_GroupsType)
  protected
    { IXMLKey_Group_GroupsType }
    function Get_Key_Group(Index: Integer): IXMLKey_GroupType;
    function Add: IXMLKey_GroupType;
    function Insert(const Index: Integer): IXMLKey_GroupType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLKey_GroupType }

  TXMLKey_GroupType = class(TXMLNode, IXMLKey_GroupType)
  protected
    { IXMLKey_GroupType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Key_GroupProps: IXMLKey_GroupPropsType;
    function Get_Key_Group_Member_Groups: IXMLKey_Group_Member_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLKey_GroupPropsType }

  TXMLKey_GroupPropsType = class(TXMLNode, IXMLKey_GroupPropsType)
  protected
    { IXMLKey_GroupPropsType }
    function Get_Key_Group_Type: WideString;
    function Get_Index_Generate: Integer;
    function Get_Key_Group_Relationship_Pointer: WideString;
    function Get_ORACLE_BITMAP: WideString;
    function Get_Index_Clustered: Integer;
    function Get_Partition_Global_Type: WideString;
    function Get_Physical_Name: WideString;
    procedure Set_Key_Group_Type(Value: WideString);
    procedure Set_Index_Generate(Value: Integer);
    procedure Set_Key_Group_Relationship_Pointer(Value: WideString);
    procedure Set_ORACLE_BITMAP(Value: WideString);
    procedure Set_Index_Clustered(Value: Integer);
    procedure Set_Partition_Global_Type(Value: WideString);
    procedure Set_Physical_Name(Value: WideString);
  end;

{ TXMLKey_Group_Member_GroupsType }

  TXMLKey_Group_Member_GroupsType = class(TXMLNodeCollection, IXMLKey_Group_Member_GroupsType)
  protected
    { IXMLKey_Group_Member_GroupsType }
    function Get_Key_Group_Member(Index: Integer): IXMLKey_Group_MemberType;
    function Add: IXMLKey_Group_MemberType;
    function Insert(const Index: Integer): IXMLKey_Group_MemberType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLKey_Group_MemberType }

  TXMLKey_Group_MemberType = class(TXMLNode, IXMLKey_Group_MemberType)
  protected
    { IXMLKey_Group_MemberType }
    function Get_Id: WideString;
    function Get_Name: Integer;
    function Get_Key_Group_MemberProps: IXMLKey_Group_MemberPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLKey_Group_MemberPropsType }

  TXMLKey_Group_MemberPropsType = class(TXMLNode, IXMLKey_Group_MemberPropsType)
  protected
    { IXMLKey_Group_MemberPropsType }
    function Get_Key_Group_Member_Column: WideString;
    function Get_Key_Group_Sort_Order: WideString;
    function Get_Key_Group_Position: Integer;
    procedure Set_Key_Group_Member_Column(Value: WideString);
    procedure Set_Key_Group_Sort_Order(Value: WideString);
    procedure Set_Key_Group_Position(Value: Integer);
  end;

{ TXMLDomain_GroupsType }

  TXMLDomain_GroupsType = class(TXMLNodeCollection, IXMLDomain_GroupsType)
  protected
    { IXMLDomain_GroupsType }
    function Get_Domain(Index: Integer): IXMLDomainType;
    function Add: IXMLDomainType;
    function Insert(const Index: Integer): IXMLDomainType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDomainType }

  TXMLDomainType = class(TXMLNode, IXMLDomainType)
  protected
    { IXMLDomainType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_DomainProps: IXMLDomainPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDomainPropsType }

  TXMLDomainPropsType = class(TXMLNode, IXMLDomainPropsType)
  protected
    { IXMLDomainPropsType }
    function Get_Type_: IXMLType_;
    function Get_Datatype: WideString;
    function Get_Domain_Icon: WideString;
    function Get_Physical_Domain_Name: IXMLPhysical_Domain_NameType;
    function Get_Parent_Domain: IXMLParent_DomainType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Datatype(Value: WideString);
    procedure Set_Domain_Icon(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPhysical_Domain_NameType }

  TXMLPhysical_Domain_NameType = class(TXMLNode, IXMLPhysical_Domain_NameType)
  protected
    { IXMLPhysical_Domain_NameType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLParent_DomainType }

  TXMLParent_DomainType = class(TXMLNode, IXMLParent_DomainType)
  protected
    { IXMLParent_DomainType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLBitmap_GroupsType }

  TXMLBitmap_GroupsType = class(TXMLNodeCollection, IXMLBitmap_GroupsType)
  protected
    { IXMLBitmap_GroupsType }
    function Get_Bitmap(Index: Integer): IXMLBitmapType;
    function Add: IXMLBitmapType;
    function Insert(const Index: Integer): IXMLBitmapType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBitmapType }

  TXMLBitmapType = class(TXMLNode, IXMLBitmapType)
  protected
    { IXMLBitmapType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_BitmapProps: IXMLBitmapPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBitmapPropsType }

  TXMLBitmapPropsType = class(TXMLNode, IXMLBitmapPropsType)
  protected
    { IXMLBitmapPropsType }
    function Get_Bitmap_Filename: WideString;
    function Get_Bitmap_DIB_Size: IXMLBitmap_DIB_SizeType;
    function Get_Bitmap_DIB_Handle: IXMLBitmap_DIB_HandleType;
    function Get_Flags: IXMLFlagsType;
    function Get_Image_Index: IXMLImage_IndexType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Bitmap_Filename(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBitmap_DIB_SizeType }

  TXMLBitmap_DIB_SizeType = class(TXMLNode, IXMLBitmap_DIB_SizeType)
  protected
    { IXMLBitmap_DIB_SizeType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLBitmap_DIB_HandleType }

  TXMLBitmap_DIB_HandleType = class(TXMLNode, IXMLBitmap_DIB_HandleType)
  protected
    { IXMLBitmap_DIB_HandleType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLFlagsType }

  TXMLFlagsType = class(TXMLNode, IXMLFlagsType)
  protected
    { IXMLFlagsType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLImage_IndexType }

  TXMLImage_IndexType = class(TXMLNode, IXMLImage_IndexType)
  protected
    { IXMLImage_IndexType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLSubject_Area_GroupsType }

  TXMLSubject_Area_GroupsType = class(TXMLNodeCollection, IXMLSubject_Area_GroupsType)
  protected
    { IXMLSubject_Area_GroupsType }
    function Get_Subject_Area(Index: Integer): IXMLSubject_AreaType;
    function Add: IXMLSubject_AreaType;
    function Insert(const Index: Integer): IXMLSubject_AreaType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSubject_AreaType }

  TXMLSubject_AreaType = class(TXMLNode, IXMLSubject_AreaType)
  protected
    { IXMLSubject_AreaType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Subject_AreaProps: IXMLSubject_AreaPropsType;
    function Get_Stored_Display_Groups: IXMLStored_Display_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSubject_AreaPropsType }

  TXMLSubject_AreaPropsType = class(TXMLNode, IXMLSubject_AreaPropsType)
  private
    FReferenced_Entities: IXMLReferenced_EntitiesTypeList;
    FReferenced_Relationships: IXMLReferenced_RelationshipsTypeList;
  protected
    { IXMLSubject_AreaPropsType }
    function Get_Name: IXMLNameType;
    function Get_Referenced_Entities: IXMLReferenced_EntitiesTypeList;
    function Get_Referenced_Relationships: IXMLReferenced_RelationshipsTypeList;
    function Get_Created_Time: Integer;
    function Get_Modified_Time: Integer;
    function Get_Is_Locked: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_Filter_Dangling_Rels_from_Schema_Gen: WideString;
    procedure Set_Created_Time(Value: Integer);
    procedure Set_Modified_Time(Value: Integer);
    procedure Set_Is_Locked(Value: WideString);
    procedure Set_Filter_Dangling_Rels_from_Schema_Gen(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNameType }

  TXMLNameType = class(TXMLNode, IXMLNameType)
  protected
    { IXMLNameType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLReferenced_EntitiesType }

  TXMLReferenced_EntitiesType = class(TXMLNode, IXMLReferenced_EntitiesType)
  protected
    { IXMLReferenced_EntitiesType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLReferenced_EntitiesTypeList }

  TXMLReferenced_EntitiesTypeList = class(TXMLNodeCollection, IXMLReferenced_EntitiesTypeList)
  protected
    { IXMLReferenced_EntitiesTypeList }
    function Add: IXMLReferenced_EntitiesType;
    function Insert(const Index: Integer): IXMLReferenced_EntitiesType;
    function Get_Item(Index: Integer): IXMLReferenced_EntitiesType;
  end;

{ TXMLReferenced_RelationshipsType }

  TXMLReferenced_RelationshipsType = class(TXMLNode, IXMLReferenced_RelationshipsType)
  protected
    { IXMLReferenced_RelationshipsType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLReferenced_RelationshipsTypeList }

  TXMLReferenced_RelationshipsTypeList = class(TXMLNodeCollection, IXMLReferenced_RelationshipsTypeList)
  protected
    { IXMLReferenced_RelationshipsTypeList }
    function Add: IXMLReferenced_RelationshipsType;
    function Insert(const Index: Integer): IXMLReferenced_RelationshipsType;
    function Get_Item(Index: Integer): IXMLReferenced_RelationshipsType;
  end;

{ TXMLStored_Display_GroupsType }

  TXMLStored_Display_GroupsType = class(TXMLNode, IXMLStored_Display_GroupsType)
  protected
    { IXMLStored_Display_GroupsType }
    function Get_Stored_Display: IXMLStored_DisplayType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStored_DisplayType }

  TXMLStored_DisplayType = class(TXMLNode, IXMLStored_DisplayType)
  protected
    { IXMLStored_DisplayType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Stored_DisplayProps: IXMLStored_DisplayPropsType;
    function Get_Drawing_Object_Entity_Groups: IXMLDrawing_Object_Entity_GroupsType;
    function Get_Drawing_Object_Relationship_Groups: IXMLDrawing_Object_Relationship_GroupsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStored_DisplayPropsType }

  TXMLStored_DisplayPropsType = class(TXMLNode, IXMLStored_DisplayPropsType)
  protected
    { IXMLStored_DisplayPropsType }
    function Get_Name: WideString;
    function Get_Display_Rel_Name: Integer;
    function Get_Display_Cardinality: Integer;
    function Get_Display_Not_Null: Integer;
    function Get_Display_Physical_Level: Integer;
    function Get_Display_Datatype: Integer;
    function Get_Display_Rolenames: Integer;
    function Get_Display_Alt_Keys: Integer;
    function Get_Display_Level: Integer;
    function Get_Zoom_Option: Integer;
    function Get_Display_FK: Integer;
    function Get_Display_Subcat_Name: Integer;
    function Get_Display_Referantial_Integrity: Integer;
    function Get_Physical_Display_Level: Integer;
    function Get_Physical_Display_FK_Name: Integer;
    function Get_Physical_Display_Cardinality: Integer;
    function Get_Physical_Display_Ref_Integ: Integer;
    function Get_Physical_Display_Alt_Keys: Integer;
    function Get_Disallow_Manual_Rel_Layout: WideString;
    function Get_Dont_Display_Views: WideString;
    function Get_Dont_Display_View_Rels: WideString;
    function Get_Display_View_Col_Datatype: WideString;
    function Get_Display_View_Col_Null_Option: WideString;
    function Get_Display_Ungenerated: WideString;
    function Get_Show_Migrated_Atts: WideString;
    function Get_Display_Logical_Datatype: WideString;
    function Get_Display_Logical_FK_Deisgnator: WideString;
    function Get_Display_Diagonal_Lines: WideString;
    function Get_Display_Attribute_Icons: WideString;
    function Get_Display_Entity_Icons: WideString;
    function Get_Display_Logical_PK_Designator: WideString;
    function Get_Display_Physical_PK_Designator: WideString;
    function Get_Display_DW_Icons: WideString;
    function Get_Display_View_Col_Expr: WideString;
    function Get_Hide_Rels: WideString;
    function Get_Display_Danging_Rels: WideString;
    function Get_Disallow_Manual_Entity_Resize: WideString;
    function Get_Shadow: WideString;
    function Get_Shadow_X: Integer;
    function Get_Shadow_Y: Integer;
    function Get_V_Scroll_Pos: Integer;
    function Get_H_Scroll_Pos: Integer;
    function Get_Modified_Time: Integer;
    function Get_Is_Locked: WideString;
    function Get_Display_Logical_Domains: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_Print_Info: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Display_Rel_Name(Value: Integer);
    procedure Set_Display_Cardinality(Value: Integer);
    procedure Set_Display_Not_Null(Value: Integer);
    procedure Set_Display_Physical_Level(Value: Integer);
    procedure Set_Display_Datatype(Value: Integer);
    procedure Set_Display_Rolenames(Value: Integer);
    procedure Set_Display_Alt_Keys(Value: Integer);
    procedure Set_Display_Level(Value: Integer);
    procedure Set_Zoom_Option(Value: Integer);
    procedure Set_Display_FK(Value: Integer);
    procedure Set_Display_Subcat_Name(Value: Integer);
    procedure Set_Display_Referantial_Integrity(Value: Integer);
    procedure Set_Physical_Display_Level(Value: Integer);
    procedure Set_Physical_Display_FK_Name(Value: Integer);
    procedure Set_Physical_Display_Cardinality(Value: Integer);
    procedure Set_Physical_Display_Ref_Integ(Value: Integer);
    procedure Set_Physical_Display_Alt_Keys(Value: Integer);
    procedure Set_Disallow_Manual_Rel_Layout(Value: WideString);
    procedure Set_Dont_Display_Views(Value: WideString);
    procedure Set_Dont_Display_View_Rels(Value: WideString);
    procedure Set_Display_View_Col_Datatype(Value: WideString);
    procedure Set_Display_View_Col_Null_Option(Value: WideString);
    procedure Set_Display_Ungenerated(Value: WideString);
    procedure Set_Show_Migrated_Atts(Value: WideString);
    procedure Set_Display_Logical_Datatype(Value: WideString);
    procedure Set_Display_Logical_FK_Deisgnator(Value: WideString);
    procedure Set_Display_Diagonal_Lines(Value: WideString);
    procedure Set_Display_Attribute_Icons(Value: WideString);
    procedure Set_Display_Entity_Icons(Value: WideString);
    procedure Set_Display_Logical_PK_Designator(Value: WideString);
    procedure Set_Display_Physical_PK_Designator(Value: WideString);
    procedure Set_Display_DW_Icons(Value: WideString);
    procedure Set_Display_View_Col_Expr(Value: WideString);
    procedure Set_Hide_Rels(Value: WideString);
    procedure Set_Display_Danging_Rels(Value: WideString);
    procedure Set_Disallow_Manual_Entity_Resize(Value: WideString);
    procedure Set_Shadow(Value: WideString);
    procedure Set_Shadow_X(Value: Integer);
    procedure Set_Shadow_Y(Value: Integer);
    procedure Set_V_Scroll_Pos(Value: Integer);
    procedure Set_H_Scroll_Pos(Value: Integer);
    procedure Set_Modified_Time(Value: Integer);
    procedure Set_Is_Locked(Value: WideString);
    procedure Set_Display_Logical_Domains(Value: WideString);
    procedure Set_Print_Info(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDrawing_Object_Entity_GroupsType }

  TXMLDrawing_Object_Entity_GroupsType = class(TXMLNodeCollection, IXMLDrawing_Object_Entity_GroupsType)
  protected
    { IXMLDrawing_Object_Entity_GroupsType }
    function Get_Drawing_Object_Entity(Index: Integer): IXMLDrawing_Object_EntityType;
    function Add: IXMLDrawing_Object_EntityType;
    function Insert(const Index: Integer): IXMLDrawing_Object_EntityType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDrawing_Object_EntityType }

  TXMLDrawing_Object_EntityType = class(TXMLNode, IXMLDrawing_Object_EntityType)
  protected
    { IXMLDrawing_Object_EntityType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Drawing_Object_EntityProps: IXMLDrawing_Object_EntityPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDrawing_Object_EntityPropsType }

  TXMLDrawing_Object_EntityPropsType = class(TXMLNode, IXMLDrawing_Object_EntityPropsType)
  protected
    { IXMLDrawing_Object_EntityPropsType }
    function Get_DO_Text: WideString;
    function Get_DO_Location: WideString;
    function Get_DO_Reference_Object: WideString;
    function Get_DO_Entity_Width_AutoResizeable: WideString;
    function Get_DO_Entity_Height_AutoResizeable: WideString;
    procedure Set_DO_Text(Value: WideString);
    procedure Set_DO_Location(Value: WideString);
    procedure Set_DO_Reference_Object(Value: WideString);
    procedure Set_DO_Entity_Width_AutoResizeable(Value: WideString);
    procedure Set_DO_Entity_Height_AutoResizeable(Value: WideString);
  end;

{ TXMLDrawing_Object_Relationship_GroupsType }

  TXMLDrawing_Object_Relationship_GroupsType = class(TXMLNode, IXMLDrawing_Object_Relationship_GroupsType)
  protected
    { IXMLDrawing_Object_Relationship_GroupsType }
    function Get_Drawing_Object_Relationship: IXMLDrawing_Object_RelationshipType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDrawing_Object_RelationshipType }

  TXMLDrawing_Object_RelationshipType = class(TXMLNode, IXMLDrawing_Object_RelationshipType)
  protected
    { IXMLDrawing_Object_RelationshipType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Drawing_Object_RelationshipProps: IXMLDrawing_Object_RelationshipPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDrawing_Object_RelationshipPropsType }

  TXMLDrawing_Object_RelationshipPropsType = class(TXMLNode, IXMLDrawing_Object_RelationshipPropsType)
  private
    FDO_Relationship_Path: IXMLString_List;
  protected
    { IXMLDrawing_Object_RelationshipPropsType }
    function Get_DO_Text: WideString;
    function Get_DO_Reference_Object: WideString;
    function Get_DO_Relationship_Path: IXMLString_List;
    function Get_DO_User_Controled_Path: WideString;
    function Get_DO_Parent_Side_Verb_Phrase: Integer;
    function Get_DO_Childside_Verb_Phrase: Integer;
    function Get_DO_Mid_Point_of_Relationship: WideString;
    function Get_DO_Parent_Side_Verb_Phrase_Coordinates: WideString;
    function Get_DO_Child_Side_Verb_Phrase_Coordinates: WideString;
    function Get_DO_Color_Inherited: WideString;
    function Get_DO_Font_Inherited: WideString;
    procedure Set_DO_Text(Value: WideString);
    procedure Set_DO_Reference_Object(Value: WideString);
    procedure Set_DO_User_Controled_Path(Value: WideString);
    procedure Set_DO_Parent_Side_Verb_Phrase(Value: Integer);
    procedure Set_DO_Childside_Verb_Phrase(Value: Integer);
    procedure Set_DO_Mid_Point_of_Relationship(Value: WideString);
    procedure Set_DO_Parent_Side_Verb_Phrase_Coordinates(Value: WideString);
    procedure Set_DO_Child_Side_Verb_Phrase_Coordinates(Value: WideString);
    procedure Set_DO_Color_Inherited(Value: WideString);
    procedure Set_DO_Font_Inherited(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRelationship_GroupsType }

  TXMLRelationship_GroupsType = class(TXMLNodeCollection, IXMLRelationship_GroupsType)
  protected
    { IXMLRelationship_GroupsType }
    function Get_Relationship(Index: Integer): IXMLRelationshipType;
    function Add: IXMLRelationshipType;
    function Insert(const Index: Integer): IXMLRelationshipType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRelationshipType }

  TXMLRelationshipType = class(TXMLNode, IXMLRelationshipType)
  protected
    { IXMLRelationshipType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_RelationshipProps: IXMLRelationshipPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRelationshipPropsType }

  TXMLRelationshipPropsType = class(TXMLNode, IXMLRelationshipPropsType)
  protected
    { IXMLRelationshipPropsType }
    function Get_Name: WideString;
    function Get_Type_: Integer;
    function Get_Note: WideString;
    function Get_Cardinality: WideString;
    function Get_Relationship_No_Nulls: Integer;
    function Get_Relationship_Sequence: Integer;
    function Get_Relationship_Parent_Update_Rule: Integer;
    function Get_Relationship_Parent_Delete_Rule: Integer;
    function Get_Relationship_Parent_Insert_Rule: Integer;
    function Get_Relationship_Child_Update_Rule: Integer;
    function Get_Relationship_Child_Delete_Rule: Integer;
    function Get_Relationship_Child_Insert_Rule: Integer;
    function Get_Physical_Only: WideString;
    function Get_Relationship_Parent_Entity: WideString;
    function Get_Relationship_Child_Entity: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Type_(Value: Integer);
    procedure Set_Note(Value: WideString);
    procedure Set_Cardinality(Value: WideString);
    procedure Set_Relationship_No_Nulls(Value: Integer);
    procedure Set_Relationship_Sequence(Value: Integer);
    procedure Set_Relationship_Parent_Update_Rule(Value: Integer);
    procedure Set_Relationship_Parent_Delete_Rule(Value: Integer);
    procedure Set_Relationship_Parent_Insert_Rule(Value: Integer);
    procedure Set_Relationship_Child_Update_Rule(Value: Integer);
    procedure Set_Relationship_Child_Delete_Rule(Value: Integer);
    procedure Set_Relationship_Child_Insert_Rule(Value: Integer);
    procedure Set_Physical_Only(Value: WideString);
    procedure Set_Relationship_Parent_Entity(Value: WideString);
    procedure Set_Relationship_Child_Entity(Value: WideString);
  end;

{ TXMLDefault_Value_GroupsType }

  TXMLDefault_Value_GroupsType = class(TXMLNodeCollection, IXMLDefault_Value_GroupsType)
  protected
    { IXMLDefault_Value_GroupsType }
    function Get_Default_Value(Index: Integer): IXMLDefault_ValueType;
    function Add: IXMLDefault_ValueType;
    function Insert(const Index: Integer): IXMLDefault_ValueType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDefault_ValueType }

  TXMLDefault_ValueType = class(TXMLNode, IXMLDefault_ValueType)
  protected
    { IXMLDefault_ValueType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Default_ValueProps: IXMLDefault_ValuePropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDefault_ValuePropsType }

  TXMLDefault_ValuePropsType = class(TXMLNode, IXMLDefault_ValuePropsType)
  protected
    { IXMLDefault_ValuePropsType }
    function Get_Name: WideString;
    function Get_Usage_Count: Integer;
    function Get_Server_Value: WideString;
    function Get_Physical_Only: WideString;
    function Get_Physical_Name: WideString;
    function Get_LogicalDefault_Value: WideString;
    function Get_Is_Builtin: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_System_Generated: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Usage_Count(Value: Integer);
    procedure Set_Server_Value(Value: WideString);
    procedure Set_Physical_Only(Value: WideString);
    procedure Set_Physical_Name(Value: WideString);
    procedure Set_LogicalDefault_Value(Value: WideString);
    procedure Set_Is_Builtin(Value: WideString);
    procedure Set_System_Generated(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTrigger_Template_GroupsType }

  TXMLTrigger_Template_GroupsType = class(TXMLNodeCollection, IXMLTrigger_Template_GroupsType)
  protected
    { IXMLTrigger_Template_GroupsType }
    function Get_Trigger_Template(Index: Integer): IXMLTrigger_TemplateType;
    function Add: IXMLTrigger_TemplateType;
    function Insert(const Index: Integer): IXMLTrigger_TemplateType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTrigger_TemplateType }

  TXMLTrigger_TemplateType = class(TXMLNode, IXMLTrigger_TemplateType)
  protected
    { IXMLTrigger_TemplateType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Trigger_TemplateProps: IXMLTrigger_TemplatePropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTrigger_TemplatePropsType }

  TXMLTrigger_TemplatePropsType = class(TXMLNode, IXMLTrigger_TemplatePropsType)
  protected
    { IXMLTrigger_TemplatePropsType }
    function Get_Name: IXMLNameType;
    function Get_Type_: IXMLType_;
    function Get_Template_Code: IXMLTemplate_CodeType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTemplate_CodeType }

  TXMLTemplate_CodeType = class(TXMLNode, IXMLTemplate_CodeType)
  protected
    { IXMLTemplate_CodeType }
    function Get_Space: WideString;
    procedure Set_Space(Value: WideString);
  end;

{ TXMLDefault_Trigger_Template_GroupsType }

  TXMLDefault_Trigger_Template_GroupsType = class(TXMLNodeCollection, IXMLDefault_Trigger_Template_GroupsType)
  protected
    { IXMLDefault_Trigger_Template_GroupsType }
    function Get_Default_Trigger_Template(Index: Integer): IXMLDefault_Trigger_TemplateType;
    function Add: IXMLDefault_Trigger_TemplateType;
    function Insert(const Index: Integer): IXMLDefault_Trigger_TemplateType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDefault_Trigger_TemplateType }

  TXMLDefault_Trigger_TemplateType = class(TXMLNode, IXMLDefault_Trigger_TemplateType)
  protected
    { IXMLDefault_Trigger_TemplateType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Default_Trigger_TemplateProps: IXMLDefault_Trigger_TemplatePropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDefault_Trigger_TemplatePropsType }

  TXMLDefault_Trigger_TemplatePropsType = class(TXMLNode, IXMLDefault_Trigger_TemplatePropsType)
  protected
    { IXMLDefault_Trigger_TemplatePropsType }
    function Get_Trigger_Template_Ref: WideString;
    function Get_Template_Purpose_Text: IXMLTemplate_Purpose_TextType;
    function Get_Template_Purpose: IXMLTemplate_PurposeType;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Trigger_Template_Ref(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTemplate_Purpose_TextType }

  TXMLTemplate_Purpose_TextType = class(TXMLNode, IXMLTemplate_Purpose_TextType)
  protected
    { IXMLTemplate_Purpose_TextType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLTemplate_PurposeType }

  TXMLTemplate_PurposeType = class(TXMLNode, IXMLTemplate_PurposeType)
  protected
    { IXMLTemplate_PurposeType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLName_Mapping_GroupsType }

  TXMLName_Mapping_GroupsType = class(TXMLNodeCollection, IXMLName_Mapping_GroupsType)
  protected
    { IXMLName_Mapping_GroupsType }
    function Get_Name_Mapping(Index: Integer): IXMLName_MappingType;
    function Add: IXMLName_MappingType;
    function Insert(const Index: Integer): IXMLName_MappingType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLName_MappingType }

  TXMLName_MappingType = class(TXMLNode, IXMLName_MappingType)
  protected
    { IXMLName_MappingType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Name_MappingProps: IXMLName_MappingPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLName_MappingPropsType }

  TXMLName_MappingPropsType = class(TXMLNode, IXMLName_MappingPropsType)
  protected
    { IXMLName_MappingPropsType }
    function Get_Name: WideString;
    function Get_Name_Mapping_Obj_Type: Integer;
    function Get_Name_Mapping_Use_Glossary: WideString;
    function Get_Name_Mapping_Alternate_Abbrev: WideString;
    function Get_Name_Mapping_Macro_Text: WideString;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    procedure Set_Name(Value: WideString);
    procedure Set_Name_Mapping_Obj_Type(Value: Integer);
    procedure Set_Name_Mapping_Use_Glossary(Value: WideString);
    procedure Set_Name_Mapping_Alternate_Abbrev(Value: WideString);
    procedure Set_Name_Mapping_Macro_Text(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNaming_Options_GroupsType }

  TXMLNaming_Options_GroupsType = class(TXMLNodeCollection, IXMLNaming_Options_GroupsType)
  protected
    { IXMLNaming_Options_GroupsType }
    function Get_Naming_Options(Index: Integer): IXMLNaming_OptionsType;
    function Add: IXMLNaming_OptionsType;
    function Insert(const Index: Integer): IXMLNaming_OptionsType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNaming_OptionsType }

  TXMLNaming_OptionsType = class(TXMLNode, IXMLNaming_OptionsType)
  protected
    { IXMLNaming_OptionsType }
    function Get_Id: WideString;
    function Get_Name: WideString;
    function Get_Naming_OptionsProps: IXMLNaming_OptionsPropsType;
    procedure Set_Id(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNaming_OptionsPropsType }

  TXMLNaming_OptionsPropsType = class(TXMLNode, IXMLNaming_OptionsPropsType)
  protected
    { IXMLNaming_OptionsPropsType }
    function Get_Name: IXMLNameType;
    function Get_Physical_Only: IXMLPhysical_OnlyType;
    function Get_Name_Mapping_Obj_Type: Integer;
    function Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
    function Get_Logical_Only: IXMLLogical_OnlyType;
    procedure Set_Name_Mapping_Obj_Type(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPhysical_OnlyType }

  TXMLPhysical_OnlyType = class(TXMLNode, IXMLPhysical_OnlyType)
  protected
    { IXMLPhysical_OnlyType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLLogical_OnlyType }

  TXMLLogical_OnlyType = class(TXMLNode, IXMLLogical_OnlyType)
  protected
    { IXMLLogical_OnlyType }
    function Get_RO: WideString;
    procedure Set_RO(Value: WideString);
  end;

{ TXMLString_List }

  TXMLString_List = class(TXMLNodeCollection, IXMLString_List)
  protected
    { IXMLString_List }
    function Add(const Value: WideString): IXMLNode;
    function Insert(const Index: Integer; const Value: WideString): IXMLNode;
    function Get_Item(Index: Integer): WideString;
  end;

{ Global Functions }

function GetERwin4(Doc: IXMLDocument): IXMLERwin4Type;
function LoadERwin4(const FileName: WideString): IXMLERwin4Type;
function NewERwin4: IXMLERwin4Type;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetERwin4(Doc: IXMLDocument): IXMLERwin4Type;
begin
  Result := Doc.GetDocBinding('ERwin4', TXMLERwin4Type, TargetNamespace) as IXMLERwin4Type;
end;

function LoadERwin4(const FileName: WideString): IXMLERwin4Type;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('ERwin4', TXMLERwin4Type, TargetNamespace) as IXMLERwin4Type;
end;

function NewERwin4: IXMLERwin4Type;
begin
  Result := NewXMLDocument.GetDocBinding('ERwin4', TXMLERwin4Type, TargetNamespace) as IXMLERwin4Type;
end;

{ TXMLERwin4Type }

procedure TXMLERwin4Type.AfterConstruction;
begin
  RegisterChildNode('Model', TXMLModelType);
  inherited;
end;

function TXMLERwin4Type.Get_FileVersion: Integer;
begin
  Result := AttributeNodes['FileVersion'].NodeValue;
end;

procedure TXMLERwin4Type.Set_FileVersion(Value: Integer);
begin
  SetAttribute('FileVersion', Value);
end;

function TXMLERwin4Type.Get_Model: IXMLModelType;
begin
  Result := ChildNodes['Model'] as IXMLModelType;
end;

{ TXMLModelType }

procedure TXMLModelType.AfterConstruction;
begin
  RegisterChildNode('ModelProps', TXMLModelPropsType);
  RegisterChildNode('Entity_Groups', TXMLEntity_GroupsType);
  RegisterChildNode('Domain_Groups', TXMLDomain_GroupsType);
  RegisterChildNode('Bitmap_Groups', TXMLBitmap_GroupsType);
  RegisterChildNode('Subject_Area_Groups', TXMLSubject_Area_GroupsType);
  RegisterChildNode('Relationship_Groups', TXMLRelationship_GroupsType);
  RegisterChildNode('Default_Value_Groups', TXMLDefault_Value_GroupsType);
  RegisterChildNode('Trigger_Template_Groups', TXMLTrigger_Template_GroupsType);
  RegisterChildNode('Default_Trigger_Template_Groups', TXMLDefault_Trigger_Template_GroupsType);
  RegisterChildNode('Name_Mapping_Groups', TXMLName_Mapping_GroupsType);
  RegisterChildNode('Naming_Options_Groups', TXMLNaming_Options_GroupsType);
  RegisterChildNode('History_Information_Groups', TXMLHistory_Information_GroupsType);
  inherited;
end;

function TXMLModelType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLModelType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLModelType.Get_ModelType: Integer;
begin
  Result := AttributeNodes['ModelType'].NodeValue;
end;

procedure TXMLModelType.Set_ModelType(Value: Integer);
begin
  SetAttribute('ModelType', Value);
end;

function TXMLModelType.Get_TargetServer: Integer;
begin
  Result := AttributeNodes['TargetServer'].NodeValue;
end;

procedure TXMLModelType.Set_TargetServer(Value: Integer);
begin
  SetAttribute('TargetServer', Value);
end;

function TXMLModelType.Get_DBMSVersion: Integer;
begin
  Result := AttributeNodes['DBMSVersion'].NodeValue;
end;

procedure TXMLModelType.Set_DBMSVersion(Value: Integer);
begin
  SetAttribute('DBMSVersion', Value);
end;

function TXMLModelType.Get_DBMSMinorVersion: Integer;
begin
  Result := AttributeNodes['DBMSMinorVersion'].NodeValue;
end;

procedure TXMLModelType.Set_DBMSMinorVersion(Value: Integer);
begin
  SetAttribute('DBMSMinorVersion', Value);
end;

function TXMLModelType.Get_ModelProps: IXMLModelPropsType;
begin
  Result := ChildNodes['ModelProps'] as IXMLModelPropsType;
end;

function TXMLModelType.Get_Entity_Groups: IXMLEntity_GroupsType;
begin
  Result := ChildNodes['Entity_Groups'] as IXMLEntity_GroupsType;
end;

function TXMLModelType.Get_Domain_Groups: IXMLDomain_GroupsType;
begin
  Result := ChildNodes['Domain_Groups'] as IXMLDomain_GroupsType;
end;

function TXMLModelType.Get_Bitmap_Groups: IXMLBitmap_GroupsType;
begin
  Result := ChildNodes['Bitmap_Groups'] as IXMLBitmap_GroupsType;
end;

function TXMLModelType.Get_Subject_Area_Groups: IXMLSubject_Area_GroupsType;
begin
  Result := ChildNodes['Subject_Area_Groups'] as IXMLSubject_Area_GroupsType;
end;

function TXMLModelType.Get_Relationship_Groups: IXMLRelationship_GroupsType;
begin
  Result := ChildNodes['Relationship_Groups'] as IXMLRelationship_GroupsType;
end;

function TXMLModelType.Get_Default_Value_Groups: IXMLDefault_Value_GroupsType;
begin
  Result := ChildNodes['Default_Value_Groups'] as IXMLDefault_Value_GroupsType;
end;

function TXMLModelType.Get_Trigger_Template_Groups: IXMLTrigger_Template_GroupsType;
begin
  Result := ChildNodes['Trigger_Template_Groups'] as IXMLTrigger_Template_GroupsType;
end;

function TXMLModelType.Get_Default_Trigger_Template_Groups: IXMLDefault_Trigger_Template_GroupsType;
begin
  Result := ChildNodes['Default_Trigger_Template_Groups'] as IXMLDefault_Trigger_Template_GroupsType;
end;

function TXMLModelType.Get_Name_Mapping_Groups: IXMLName_Mapping_GroupsType;
begin
  Result := ChildNodes['Name_Mapping_Groups'] as IXMLName_Mapping_GroupsType;
end;

function TXMLModelType.Get_Naming_Options_Groups: IXMLNaming_Options_GroupsType;
begin
  Result := ChildNodes['Naming_Options_Groups'] as IXMLNaming_Options_GroupsType;
end;

function TXMLModelType.Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
begin
  Result := ChildNodes['History_Information_Groups'] as IXMLHistory_Information_GroupsType;
end;

{ TXMLModelPropsType }

procedure TXMLModelPropsType.AfterConstruction;
begin
  RegisterChildNode('Type', TXMLType_);
  RegisterChildNode('File_Name', TXMLFile_NameType);
  RegisterChildNode('File_Format', TXMLFile_FormatType);
  RegisterChildNode('Target_Server', TXMLTarget_ServerType);
  RegisterChildNode('DBMS_Version', TXMLDBMS_VersionType);
  RegisterChildNode('DBMS_Minor_Version', TXMLDBMS_Minor_VersionType);
  inherited;
end;

function TXMLModelPropsType.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLModelPropsType.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Type_: IXMLType_;
begin
  Result := ChildNodes['Type'] as IXMLType_;
end;

function TXMLModelPropsType.Get_Tracking_History_Objects: WideString;
begin
  Result := ChildNodes['Tracking_History_Objects'].Text;
end;

procedure TXMLModelPropsType.Set_Tracking_History_Objects(Value: WideString);
begin
  ChildNodes['Tracking_History_Objects'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Tracking_History_Events: WideString;
begin
  Result := ChildNodes['Tracking_History_Events'].Text;
end;

procedure TXMLModelPropsType.Set_Tracking_History_Events(Value: WideString);
begin
  ChildNodes['Tracking_History_Events'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_File_Name: IXMLFile_NameType;
begin
  Result := ChildNodes['File_Name'] as IXMLFile_NameType;
end;

function TXMLModelPropsType.Get_Page_Grid: Integer;
begin
  Result := ChildNodes['Page_Grid'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Page_Grid(Value: Integer);
begin
  ChildNodes['Page_Grid'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_File_Format: IXMLFile_FormatType;
begin
  Result := ChildNodes['File_Format'] as IXMLFile_FormatType;
end;

function TXMLModelPropsType.Get_Entity_Width: Integer;
begin
  Result := ChildNodes['Entity_Width'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Entity_Width(Value: Integer);
begin
  ChildNodes['Entity_Width'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Entity_Height: Integer;
begin
  Result := ChildNodes['Entity_Height'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Entity_Height(Value: Integer);
begin
  ChildNodes['Entity_Height'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Layout_Grid: Integer;
begin
  Result := ChildNodes['Layout_Grid'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Layout_Grid(Value: Integer);
begin
  ChildNodes['Layout_Grid'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Layout_Grid_X: Integer;
begin
  Result := ChildNodes['Layout_Grid_X'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Layout_Grid_X(Value: Integer);
begin
  ChildNodes['Layout_Grid_X'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Layout_Grid_Y: Integer;
begin
  Result := ChildNodes['Layout_Grid_Y'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Layout_Grid_Y(Value: Integer);
begin
  ChildNodes['Layout_Grid_Y'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Font_Height: Integer;
begin
  Result := ChildNodes['Font_Height'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Font_Height(Value: Integer);
begin
  ChildNodes['Font_Height'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Font_Width: Integer;
begin
  Result := ChildNodes['Font_Width'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Font_Width(Value: Integer);
begin
  ChildNodes['Font_Width'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Unique_Names: Integer;
begin
  Result := ChildNodes['Unique_Names'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Unique_Names(Value: Integer);
begin
  ChildNodes['Unique_Names'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Target_Server: IXMLTarget_ServerType;
begin
  Result := ChildNodes['Target_Server'] as IXMLTarget_ServerType;
end;

function TXMLModelPropsType.Get_Default_Datatype: WideString;
begin
  Result := ChildNodes['Default_Datatype'].Text;
end;

procedure TXMLModelPropsType.Set_Default_Datatype(Value: WideString);
begin
  ChildNodes['Default_Datatype'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Non_Key_Null: Integer;
begin
  Result := ChildNodes['Non_Key_Null'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Non_Key_Null(Value: Integer);
begin
  ChildNodes['Non_Key_Null'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Default_Fonts_and_Colors: WideString;
begin
  Result := ChildNodes['Default_Fonts_and_Colors'].Text;
end;

procedure TXMLModelPropsType.Set_Default_Fonts_and_Colors(Value: WideString);
begin
  ChildNodes['Default_Fonts_and_Colors'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Repos: Integer;
begin
  Result := ChildNodes['Repos'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Repos(Value: Integer);
begin
  ChildNodes['Repos'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_DBMS_Version: IXMLDBMS_VersionType;
begin
  Result := ChildNodes['DBMS_Version'] as IXMLDBMS_VersionType;
end;

function TXMLModelPropsType.Get_Logical_Notation: Integer;
begin
  Result := ChildNodes['Logical_Notation'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Logical_Notation(Value: Integer);
begin
  ChildNodes['Logical_Notation'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_DBMS_Minor_Version: IXMLDBMS_Minor_VersionType;
begin
  Result := ChildNodes['DBMS_Minor_Version'] as IXMLDBMS_Minor_VersionType;
end;

function TXMLModelPropsType.Get_Old_Repos: Integer;
begin
  Result := ChildNodes['Old_Repos'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Old_Repos(Value: Integer);
begin
  ChildNodes['Old_Repos'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Max_View_Expr_Display_Len: Integer;
begin
  Result := ChildNodes['Max_View_Expr_Display_Len'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Max_View_Expr_Display_Len(Value: Integer);
begin
  ChildNodes['Max_View_Expr_Display_Len'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Physical_Notation: Integer;
begin
  Result := ChildNodes['Physical_Notation'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Physical_Notation(Value: Integer);
begin
  ChildNodes['Physical_Notation'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Index_Name_Macro: WideString;
begin
  Result := ChildNodes['Index_Name_Macro'].Text;
end;

procedure TXMLModelPropsType.Set_Index_Name_Macro(Value: WideString);
begin
  ChildNodes['Index_Name_Macro'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Max_Def_Display_Len: Integer;
begin
  Result := ChildNodes['Max_Def_Display_Len'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Max_Def_Display_Len(Value: Integer);
begin
  ChildNodes['Max_Def_Display_Len'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Table_Name_Macro: WideString;
begin
  Result := ChildNodes['Table_Name_Macro'].Text;
end;

procedure TXMLModelPropsType.Set_Table_Name_Macro(Value: WideString);
begin
  ChildNodes['Table_Name_Macro'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Current_Tool: WideString;
begin
  Result := ChildNodes['Current_Tool'].Text;
end;

procedure TXMLModelPropsType.Set_Current_Tool(Value: WideString);
begin
  ChildNodes['Current_Tool'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_MM_Preview: WideString;
begin
  Result := ChildNodes['MM_Preview'].Text;
end;

procedure TXMLModelPropsType.Set_MM_Preview(Value: WideString);
begin
  ChildNodes['MM_Preview'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Saved_From_Previous_Version: WideString;
begin
  Result := ChildNodes['Saved_From_Previous_Version'].Text;
end;

procedure TXMLModelPropsType.Set_Saved_From_Previous_Version(Value: WideString);
begin
  ChildNodes['Saved_From_Previous_Version'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_WYSIWYG_Print: WideString;
begin
  Result := ChildNodes['WYSIWYG_Print'].Text;
end;

procedure TXMLModelPropsType.Set_WYSIWYG_Print(Value: WideString);
begin
  ChildNodes['WYSIWYG_Print'].NodeValue := Value;
end;

function TXMLModelPropsType.Get_Model_Background_Color: Integer;
begin
  Result := ChildNodes['Model_Background_Color'].NodeValue;
end;

procedure TXMLModelPropsType.Set_Model_Background_Color(Value: Integer);
begin
  ChildNodes['Model_Background_Color'].NodeValue := Value;
end;

{ TXMLType_ }

function TXMLType_.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLType_.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLFile_NameType }

function TXMLFile_NameType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLFile_NameType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLFile_FormatType }

function TXMLFile_FormatType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLFile_FormatType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLTarget_ServerType }

function TXMLTarget_ServerType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLTarget_ServerType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLDBMS_VersionType }

function TXMLDBMS_VersionType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLDBMS_VersionType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLDBMS_Minor_VersionType }

function TXMLDBMS_Minor_VersionType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLDBMS_Minor_VersionType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLEntity_GroupsType }

procedure TXMLEntity_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Entity', TXMLEntityType);
  ItemTag := 'Entity';
  ItemInterface := IXMLEntityType;
  inherited;
end;

function TXMLEntity_GroupsType.Get_Entity(Index: Integer): IXMLEntityType;
begin
  Result := List[Index] as IXMLEntityType;
end;

function TXMLEntity_GroupsType.Add: IXMLEntityType;
begin
  Result := AddItem(-1) as IXMLEntityType;
end;

function TXMLEntity_GroupsType.Insert(const Index: Integer): IXMLEntityType;
begin
  Result := AddItem(Index) as IXMLEntityType;
end;

{ TXMLEntityType }

procedure TXMLEntityType.AfterConstruction;
begin
  RegisterChildNode('EntityProps', TXMLEntityPropsType);
  RegisterChildNode('Attribute_Groups', TXMLAttribute_GroupsType);
  RegisterChildNode('Key_Group_Groups', TXMLKey_Group_GroupsType);
  RegisterChildNode('History_Information_Groups', TXMLHistory_Information_GroupsType);
  inherited;
end;

function TXMLEntityType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLEntityType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLEntityType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLEntityType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLEntityType.Get_EntityProps: IXMLEntityPropsType;
begin
  Result := ChildNodes['EntityProps'] as IXMLEntityPropsType;
end;

function TXMLEntityType.Get_Attribute_Groups: IXMLAttribute_GroupsType;
begin
  Result := ChildNodes['Attribute_Groups'] as IXMLAttribute_GroupsType;
end;

function TXMLEntityType.Get_Key_Group_Groups: IXMLKey_Group_GroupsType;
begin
  Result := ChildNodes['Key_Group_Groups'] as IXMLKey_Group_GroupsType;
end;

function TXMLEntityType.Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
begin
  Result := ChildNodes['History_Information_Groups'] as IXMLHistory_Information_GroupsType;
end;

{ TXMLEntityPropsType }

function TXMLEntityPropsType.Get_Type_: Integer;
begin
  Result := ChildNodes['Type'].NodeValue;
end;

procedure TXMLEntityPropsType.Set_Type_(Value: Integer);
begin
  ChildNodes['Type'].NodeValue := Value;
end;

function TXMLEntityPropsType.Get_Index_Generate: Integer;
begin
  Result := ChildNodes['Index_Generate'].NodeValue;
end;

procedure TXMLEntityPropsType.Set_Index_Generate(Value: Integer);
begin
  ChildNodes['Index_Generate'].NodeValue := Value;
end;

function TXMLEntityPropsType.Get_Physical_Name: WideString;
begin
  Result := ChildNodes['Physical_Name'].Text;
end;

procedure TXMLEntityPropsType.Set_Physical_Name(Value: WideString);
begin
  ChildNodes['Physical_Name'].NodeValue := Value;
end;

function TXMLEntityPropsType.Get_Comment: WideString;
begin
  Result := ChildNodes['Comment'].Text;
end;

procedure TXMLEntityPropsType.Set_Comment(Value: WideString);
begin
  ChildNodes['Comment'].NodeValue := Value;
end;

function TXMLEntityPropsType.Get_Physical_Only: WideString;
begin
  Result := ChildNodes['Physical_Only'].Text;
end;

procedure TXMLEntityPropsType.Set_Physical_Only(Value: WideString);
begin
  ChildNodes['Physical_Only'].NodeValue := Value;
end;

{ TXMLAttribute_GroupsType }

procedure TXMLAttribute_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Attribute', TXMLAttributeType);
  ItemTag := 'Attribute';
  ItemInterface := IXMLAttributeType;
  inherited;
end;

function TXMLAttribute_GroupsType.Get_Attribute(Index: Integer): IXMLAttributeType;
begin
  Result := List[Index] as IXMLAttributeType;
end;

function TXMLAttribute_GroupsType.Add: IXMLAttributeType;
begin
  Result := AddItem(-1) as IXMLAttributeType;
end;

function TXMLAttribute_GroupsType.Insert(const Index: Integer): IXMLAttributeType;
begin
  Result := AddItem(Index) as IXMLAttributeType;
end;

{ TXMLAttributeType }

procedure TXMLAttributeType.AfterConstruction;
begin
  RegisterChildNode('AttributeProps', TXMLAttributePropsType);
  RegisterChildNode('History_Information_Groups', TXMLHistory_Information_GroupsType);
  inherited;
end;

function TXMLAttributeType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLAttributeType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLAttributeType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLAttributeType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLAttributeType.Get_AttributeProps: IXMLAttributePropsType;
begin
  Result := ChildNodes['AttributeProps'] as IXMLAttributePropsType;
end;

function TXMLAttributeType.Get_History_Information_Groups: IXMLHistory_Information_GroupsType;
begin
  Result := ChildNodes['History_Information_Groups'] as IXMLHistory_Information_GroupsType;
end;

{ TXMLAttributePropsType }

procedure TXMLAttributePropsType.AfterConstruction;
begin
  RegisterChildNode('Master_Attribute', TXMLMaster_AttributeType);
  inherited;
end;

function TXMLAttributePropsType.Get_Type_: Integer;
begin
  Result := ChildNodes['Type'].NodeValue;
end;

procedure TXMLAttributePropsType.Set_Type_(Value: Integer);
begin
  ChildNodes['Type'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Null_Option: Integer;
begin
  Result := ChildNodes['Null_Option'].NodeValue;
end;

procedure TXMLAttributePropsType.Set_Null_Option(Value: Integer);
begin
  ChildNodes['Null_Option'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Order: Integer;
begin
  Result := ChildNodes['Order'].NodeValue;
end;

procedure TXMLAttributePropsType.Set_Order(Value: Integer);
begin
  ChildNodes['Order'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Physical_Order: Integer;
begin
  Result := ChildNodes['Physical_Order'].NodeValue;
end;

procedure TXMLAttributePropsType.Set_Physical_Order(Value: Integer);
begin
  ChildNodes['Physical_Order'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Parent_Attribute: WideString;
begin
  Result := ChildNodes['Parent_Attribute'].Text;
end;

procedure TXMLAttributePropsType.Set_Parent_Attribute(Value: WideString);
begin
  ChildNodes['Parent_Attribute'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Parent_Relationship: WideString;
begin
  Result := ChildNodes['Parent_Relationship'].Text;
end;

procedure TXMLAttributePropsType.Set_Parent_Relationship(Value: WideString);
begin
  ChildNodes['Parent_Relationship'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Physical_Name: WideString;
begin
  Result := ChildNodes['Physical_Name'].Text;
end;

procedure TXMLAttributePropsType.Set_Physical_Name(Value: WideString);
begin
  ChildNodes['Physical_Name'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Parent_Domain: WideString;
begin
  Result := ChildNodes['Parent_Domain'].Text;
end;

procedure TXMLAttributePropsType.Set_Parent_Domain(Value: WideString);
begin
  ChildNodes['Parent_Domain'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Hide_in_Logical: WideString;
begin
  Result := ChildNodes['Hide_in_Logical'].Text;
end;

procedure TXMLAttributePropsType.Set_Hide_in_Logical(Value: WideString);
begin
  ChildNodes['Hide_in_Logical'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Hide_in_Physical: WideString;
begin
  Result := ChildNodes['Hide_in_Physical'].Text;
end;

procedure TXMLAttributePropsType.Set_Hide_in_Physical(Value: WideString);
begin
  ChildNodes['Hide_in_Physical'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Master_Attribute: IXMLMaster_AttributeType;
begin
  Result := ChildNodes['Master_Attribute'] as IXMLMaster_AttributeType;
end;

function TXMLAttributePropsType.Get_DO_Color_Inherited: WideString;
begin
  Result := ChildNodes['DO_Color_Inherited'].Text;
end;

procedure TXMLAttributePropsType.Set_DO_Color_Inherited(Value: WideString);
begin
  ChildNodes['DO_Color_Inherited'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_DO_Font_Inherited: WideString;
begin
  Result := ChildNodes['DO_Font_Inherited'].Text;
end;

procedure TXMLAttributePropsType.Set_DO_Font_Inherited(Value: WideString);
begin
  ChildNodes['DO_Font_Inherited'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Datatype: WideString;
begin
  Result := ChildNodes['Datatype'].Text;
end;

procedure TXMLAttributePropsType.Set_Datatype(Value: WideString);
begin
  ChildNodes['Datatype'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Comment: WideString;
begin
  Result := ChildNodes['Comment'].Text;
end;

procedure TXMLAttributePropsType.Set_Comment(Value: WideString);
begin
  ChildNodes['Comment'].NodeValue := Value;
end;

function TXMLAttributePropsType.Get_Default: WideString;
begin
  Result := ChildNodes['Default'].Text;
end;

procedure TXMLAttributePropsType.Set_Default(Value: WideString);
begin
  ChildNodes['Default'].NodeValue := Value;
end;

{ TXMLMaster_AttributeType }

function TXMLMaster_AttributeType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLMaster_AttributeType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLHistory_Information_GroupsType }

procedure TXMLHistory_Information_GroupsType.AfterConstruction;
begin
  RegisterChildNode('History_Information', TXMLHistory_InformationType);
  inherited;
end;

function TXMLHistory_Information_GroupsType.Get_History_Information: IXMLHistory_InformationType;
begin
  Result := ChildNodes['History_Information'] as IXMLHistory_InformationType;
end;

{ TXMLHistory_InformationType }

procedure TXMLHistory_InformationType.AfterConstruction;
begin
  RegisterChildNode('History_InformationProps', TXMLHistory_InformationPropsType);
  inherited;
end;

function TXMLHistory_InformationType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLHistory_InformationType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLHistory_InformationType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLHistory_InformationType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLHistory_InformationType.Get_History_InformationProps: IXMLHistory_InformationPropsType;
begin
  Result := ChildNodes['History_InformationProps'] as IXMLHistory_InformationPropsType;
end;

{ TXMLHistory_InformationPropsType }

procedure TXMLHistory_InformationPropsType.AfterConstruction;
begin
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLHistory_InformationPropsType.Get_Type_: Integer;
begin
  Result := ChildNodes['Type'].NodeValue;
end;

procedure TXMLHistory_InformationPropsType.Set_Type_(Value: Integer);
begin
  ChildNodes['Type'].NodeValue := Value;
end;

function TXMLHistory_InformationPropsType.Get_Created_Time: Integer;
begin
  Result := ChildNodes['Created_Time'].NodeValue;
end;

procedure TXMLHistory_InformationPropsType.Set_Created_Time(Value: Integer);
begin
  ChildNodes['Created_Time'].NodeValue := Value;
end;

function TXMLHistory_InformationPropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

{ TXMLDefault_Object_Alternative_IdType }

function TXMLDefault_Object_Alternative_IdType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLDefault_Object_Alternative_IdType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLKey_Group_GroupsType }

procedure TXMLKey_Group_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Key_Group', TXMLKey_GroupType);
  ItemTag := 'Key_Group';
  ItemInterface := IXMLKey_GroupType;
  inherited;
end;

function TXMLKey_Group_GroupsType.Get_Key_Group(Index: Integer): IXMLKey_GroupType;
begin
  Result := List[Index] as IXMLKey_GroupType;
end;

function TXMLKey_Group_GroupsType.Add: IXMLKey_GroupType;
begin
  Result := AddItem(-1) as IXMLKey_GroupType;
end;

function TXMLKey_Group_GroupsType.Insert(const Index: Integer): IXMLKey_GroupType;
begin
  Result := AddItem(Index) as IXMLKey_GroupType;
end;

{ TXMLKey_GroupType }

procedure TXMLKey_GroupType.AfterConstruction;
begin
  RegisterChildNode('Key_GroupProps', TXMLKey_GroupPropsType);
  RegisterChildNode('Key_Group_Member_Groups', TXMLKey_Group_Member_GroupsType);
  inherited;
end;

function TXMLKey_GroupType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLKey_GroupType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLKey_GroupType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLKey_GroupType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLKey_GroupType.Get_Key_GroupProps: IXMLKey_GroupPropsType;
begin
  Result := ChildNodes['Key_GroupProps'] as IXMLKey_GroupPropsType;
end;

function TXMLKey_GroupType.Get_Key_Group_Member_Groups: IXMLKey_Group_Member_GroupsType;
begin
  Result := ChildNodes['Key_Group_Member_Groups'] as IXMLKey_Group_Member_GroupsType;
end;

{ TXMLKey_GroupPropsType }

function TXMLKey_GroupPropsType.Get_Key_Group_Type: WideString;
begin
  Result := ChildNodes['Key_Group_Type'].Text;
end;

procedure TXMLKey_GroupPropsType.Set_Key_Group_Type(Value: WideString);
begin
  ChildNodes['Key_Group_Type'].NodeValue := Value;
end;

function TXMLKey_GroupPropsType.Get_Index_Generate: Integer;
begin
  Result := ChildNodes['Index_Generate'].NodeValue;
end;

procedure TXMLKey_GroupPropsType.Set_Index_Generate(Value: Integer);
begin
  ChildNodes['Index_Generate'].NodeValue := Value;
end;

function TXMLKey_GroupPropsType.Get_Key_Group_Relationship_Pointer: WideString;
begin
  Result := ChildNodes['Key_Group_Relationship_Pointer'].Text;
end;

procedure TXMLKey_GroupPropsType.Set_Key_Group_Relationship_Pointer(Value: WideString);
begin
  ChildNodes['Key_Group_Relationship_Pointer'].NodeValue := Value;
end;

function TXMLKey_GroupPropsType.Get_ORACLE_BITMAP: WideString;
begin
  Result := ChildNodes['ORACLE_BITMAP'].Text;
end;

procedure TXMLKey_GroupPropsType.Set_ORACLE_BITMAP(Value: WideString);
begin
  ChildNodes['ORACLE_BITMAP'].NodeValue := Value;
end;

function TXMLKey_GroupPropsType.Get_Index_Clustered: Integer;
begin
  Result := ChildNodes['Index_Clustered'].NodeValue;
end;

procedure TXMLKey_GroupPropsType.Set_Index_Clustered(Value: Integer);
begin
  ChildNodes['Index_Clustered'].NodeValue := Value;
end;

function TXMLKey_GroupPropsType.Get_Partition_Global_Type: WideString;
begin
  Result := ChildNodes['Partition_Global_Type'].Text;
end;

procedure TXMLKey_GroupPropsType.Set_Partition_Global_Type(Value: WideString);
begin
  ChildNodes['Partition_Global_Type'].NodeValue := Value;
end;

function TXMLKey_GroupPropsType.Get_Physical_Name: WideString;
begin
  Result := ChildNodes['Physical_Name'].Text;
end;

procedure TXMLKey_GroupPropsType.Set_Physical_Name(Value: WideString);
begin
  ChildNodes['Physical_Name'].NodeValue := Value;
end;

{ TXMLKey_Group_Member_GroupsType }

procedure TXMLKey_Group_Member_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Key_Group_Member', TXMLKey_Group_MemberType);
  ItemTag := 'Key_Group_Member';
  ItemInterface := IXMLKey_Group_MemberType;
  inherited;
end;

function TXMLKey_Group_Member_GroupsType.Get_Key_Group_Member(Index: Integer): IXMLKey_Group_MemberType;
begin
  Result := List[Index] as IXMLKey_Group_MemberType;
end;

function TXMLKey_Group_Member_GroupsType.Add: IXMLKey_Group_MemberType;
begin
  Result := AddItem(-1) as IXMLKey_Group_MemberType;
end;

function TXMLKey_Group_Member_GroupsType.Insert(const Index: Integer): IXMLKey_Group_MemberType;
begin
  Result := AddItem(Index) as IXMLKey_Group_MemberType;
end;

{ TXMLKey_Group_MemberType }

procedure TXMLKey_Group_MemberType.AfterConstruction;
begin
  RegisterChildNode('Key_Group_MemberProps', TXMLKey_Group_MemberPropsType);
  inherited;
end;

function TXMLKey_Group_MemberType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLKey_Group_MemberType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLKey_Group_MemberType.Get_Name: Integer;
begin
  Result := AttributeNodes['Name'].NodeValue;
end;

procedure TXMLKey_Group_MemberType.Set_Name(Value: Integer);
begin
  SetAttribute('Name', Value);
end;

function TXMLKey_Group_MemberType.Get_Key_Group_MemberProps: IXMLKey_Group_MemberPropsType;
begin
  Result := ChildNodes['Key_Group_MemberProps'] as IXMLKey_Group_MemberPropsType;
end;

{ TXMLKey_Group_MemberPropsType }

function TXMLKey_Group_MemberPropsType.Get_Key_Group_Member_Column: WideString;
begin
  Result := ChildNodes['Key_Group_Member_Column'].Text;
end;

procedure TXMLKey_Group_MemberPropsType.Set_Key_Group_Member_Column(Value: WideString);
begin
  ChildNodes['Key_Group_Member_Column'].NodeValue := Value;
end;

function TXMLKey_Group_MemberPropsType.Get_Key_Group_Sort_Order: WideString;
begin
  Result := ChildNodes['Key_Group_Sort_Order'].Text;
end;

procedure TXMLKey_Group_MemberPropsType.Set_Key_Group_Sort_Order(Value: WideString);
begin
  ChildNodes['Key_Group_Sort_Order'].NodeValue := Value;
end;

function TXMLKey_Group_MemberPropsType.Get_Key_Group_Position: Integer;
begin
  Result := ChildNodes['Key_Group_Position'].NodeValue;
end;

procedure TXMLKey_Group_MemberPropsType.Set_Key_Group_Position(Value: Integer);
begin
  ChildNodes['Key_Group_Position'].NodeValue := Value;
end;

{ TXMLDomain_GroupsType }

procedure TXMLDomain_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Domain', TXMLDomainType);
  ItemTag := 'Domain';
  ItemInterface := IXMLDomainType;
  inherited;
end;

function TXMLDomain_GroupsType.Get_Domain(Index: Integer): IXMLDomainType;
begin
  Result := List[Index] as IXMLDomainType;
end;

function TXMLDomain_GroupsType.Add: IXMLDomainType;
begin
  Result := AddItem(-1) as IXMLDomainType;
end;

function TXMLDomain_GroupsType.Insert(const Index: Integer): IXMLDomainType;
begin
  Result := AddItem(Index) as IXMLDomainType;
end;

{ TXMLDomainType }

procedure TXMLDomainType.AfterConstruction;
begin
  RegisterChildNode('DomainProps', TXMLDomainPropsType);
  inherited;
end;

function TXMLDomainType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDomainType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLDomainType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLDomainType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLDomainType.Get_DomainProps: IXMLDomainPropsType;
begin
  Result := ChildNodes['DomainProps'] as IXMLDomainPropsType;
end;

{ TXMLDomainPropsType }

procedure TXMLDomainPropsType.AfterConstruction;
begin
  RegisterChildNode('Type', TXMLType_);
  RegisterChildNode('Physical_Domain_Name', TXMLPhysical_Domain_NameType);
  RegisterChildNode('Parent_Domain', TXMLParent_DomainType);
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLDomainPropsType.Get_Type_: IXMLType_;
begin
  Result := ChildNodes['Type'] as IXMLType_;
end;

function TXMLDomainPropsType.Get_Datatype: WideString;
begin
  Result := ChildNodes['Datatype'].Text;
end;

procedure TXMLDomainPropsType.Set_Datatype(Value: WideString);
begin
  ChildNodes['Datatype'].NodeValue := Value;
end;

function TXMLDomainPropsType.Get_Domain_Icon: WideString;
begin
  Result := ChildNodes['Domain_Icon'].Text;
end;

procedure TXMLDomainPropsType.Set_Domain_Icon(Value: WideString);
begin
  ChildNodes['Domain_Icon'].NodeValue := Value;
end;

function TXMLDomainPropsType.Get_Physical_Domain_Name: IXMLPhysical_Domain_NameType;
begin
  Result := ChildNodes['Physical_Domain_Name'] as IXMLPhysical_Domain_NameType;
end;

function TXMLDomainPropsType.Get_Parent_Domain: IXMLParent_DomainType;
begin
  Result := ChildNodes['Parent_Domain'] as IXMLParent_DomainType;
end;

function TXMLDomainPropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

{ TXMLPhysical_Domain_NameType }

function TXMLPhysical_Domain_NameType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLPhysical_Domain_NameType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLParent_DomainType }

function TXMLParent_DomainType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLParent_DomainType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLBitmap_GroupsType }

procedure TXMLBitmap_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Bitmap', TXMLBitmapType);
  ItemTag := 'Bitmap';
  ItemInterface := IXMLBitmapType;
  inherited;
end;

function TXMLBitmap_GroupsType.Get_Bitmap(Index: Integer): IXMLBitmapType;
begin
  Result := List[Index] as IXMLBitmapType;
end;

function TXMLBitmap_GroupsType.Add: IXMLBitmapType;
begin
  Result := AddItem(-1) as IXMLBitmapType;
end;

function TXMLBitmap_GroupsType.Insert(const Index: Integer): IXMLBitmapType;
begin
  Result := AddItem(Index) as IXMLBitmapType;
end;

{ TXMLBitmapType }

procedure TXMLBitmapType.AfterConstruction;
begin
  RegisterChildNode('BitmapProps', TXMLBitmapPropsType);
  inherited;
end;

function TXMLBitmapType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLBitmapType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLBitmapType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLBitmapType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLBitmapType.Get_BitmapProps: IXMLBitmapPropsType;
begin
  Result := ChildNodes['BitmapProps'] as IXMLBitmapPropsType;
end;

{ TXMLBitmapPropsType }

procedure TXMLBitmapPropsType.AfterConstruction;
begin
  RegisterChildNode('Bitmap_DIB_Size', TXMLBitmap_DIB_SizeType);
  RegisterChildNode('Bitmap_DIB_Handle', TXMLBitmap_DIB_HandleType);
  RegisterChildNode('Flags', TXMLFlagsType);
  RegisterChildNode('Image_Index', TXMLImage_IndexType);
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLBitmapPropsType.Get_Bitmap_Filename: WideString;
begin
  Result := ChildNodes['Bitmap_Filename'].Text;
end;

procedure TXMLBitmapPropsType.Set_Bitmap_Filename(Value: WideString);
begin
  ChildNodes['Bitmap_Filename'].NodeValue := Value;
end;

function TXMLBitmapPropsType.Get_Bitmap_DIB_Size: IXMLBitmap_DIB_SizeType;
begin
  Result := ChildNodes['Bitmap_DIB_Size'] as IXMLBitmap_DIB_SizeType;
end;

function TXMLBitmapPropsType.Get_Bitmap_DIB_Handle: IXMLBitmap_DIB_HandleType;
begin
  Result := ChildNodes['Bitmap_DIB_Handle'] as IXMLBitmap_DIB_HandleType;
end;

function TXMLBitmapPropsType.Get_Flags: IXMLFlagsType;
begin
  Result := ChildNodes['Flags'] as IXMLFlagsType;
end;

function TXMLBitmapPropsType.Get_Image_Index: IXMLImage_IndexType;
begin
  Result := ChildNodes['Image_Index'] as IXMLImage_IndexType;
end;

function TXMLBitmapPropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

{ TXMLBitmap_DIB_SizeType }

function TXMLBitmap_DIB_SizeType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLBitmap_DIB_SizeType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLBitmap_DIB_HandleType }

function TXMLBitmap_DIB_HandleType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLBitmap_DIB_HandleType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLFlagsType }

function TXMLFlagsType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLFlagsType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLImage_IndexType }

function TXMLImage_IndexType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLImage_IndexType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLSubject_Area_GroupsType }

procedure TXMLSubject_Area_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Subject_Area', TXMLSubject_AreaType);
  ItemTag := 'Subject_Area';
  ItemInterface := IXMLSubject_AreaType;
  inherited;
end;

function TXMLSubject_Area_GroupsType.Get_Subject_Area(Index: Integer): IXMLSubject_AreaType;
begin
  Result := List[Index] as IXMLSubject_AreaType;
end;

function TXMLSubject_Area_GroupsType.Add: IXMLSubject_AreaType;
begin
  Result := AddItem(-1) as IXMLSubject_AreaType;
end;

function TXMLSubject_Area_GroupsType.Insert(const Index: Integer): IXMLSubject_AreaType;
begin
  Result := AddItem(Index) as IXMLSubject_AreaType;
end;

{ TXMLSubject_AreaType }

procedure TXMLSubject_AreaType.AfterConstruction;
begin
  RegisterChildNode('Subject_AreaProps', TXMLSubject_AreaPropsType);
  RegisterChildNode('Stored_Display_Groups', TXMLStored_Display_GroupsType);
  inherited;
end;

function TXMLSubject_AreaType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLSubject_AreaType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLSubject_AreaType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLSubject_AreaType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLSubject_AreaType.Get_Subject_AreaProps: IXMLSubject_AreaPropsType;
begin
  Result := ChildNodes['Subject_AreaProps'] as IXMLSubject_AreaPropsType;
end;

function TXMLSubject_AreaType.Get_Stored_Display_Groups: IXMLStored_Display_GroupsType;
begin
  Result := ChildNodes['Stored_Display_Groups'] as IXMLStored_Display_GroupsType;
end;

{ TXMLSubject_AreaPropsType }

procedure TXMLSubject_AreaPropsType.AfterConstruction;
begin
  RegisterChildNode('Name', TXMLNameType);
  RegisterChildNode('Referenced_Entities', TXMLReferenced_EntitiesType);
  RegisterChildNode('Referenced_Relationships', TXMLReferenced_RelationshipsType);
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  FReferenced_Entities := CreateCollection(TXMLReferenced_EntitiesTypeList, IXMLReferenced_EntitiesType, 'Referenced_Entities') as IXMLReferenced_EntitiesTypeList;
  FReferenced_Relationships := CreateCollection(TXMLReferenced_RelationshipsTypeList, IXMLReferenced_RelationshipsType, 'Referenced_Relationships') as IXMLReferenced_RelationshipsTypeList;
  inherited;
end;

function TXMLSubject_AreaPropsType.Get_Name: IXMLNameType;
begin
  Result := ChildNodes['Name'] as IXMLNameType;
end;

function TXMLSubject_AreaPropsType.Get_Referenced_Entities: IXMLReferenced_EntitiesTypeList;
begin
  Result := FReferenced_Entities;
end;

function TXMLSubject_AreaPropsType.Get_Referenced_Relationships: IXMLReferenced_RelationshipsTypeList;
begin
  Result := FReferenced_Relationships;
end;

function TXMLSubject_AreaPropsType.Get_Created_Time: Integer;
begin
  Result := ChildNodes['Created_Time'].NodeValue;
end;

procedure TXMLSubject_AreaPropsType.Set_Created_Time(Value: Integer);
begin
  ChildNodes['Created_Time'].NodeValue := Value;
end;

function TXMLSubject_AreaPropsType.Get_Modified_Time: Integer;
begin
  Result := ChildNodes['Modified_Time'].NodeValue;
end;

procedure TXMLSubject_AreaPropsType.Set_Modified_Time(Value: Integer);
begin
  ChildNodes['Modified_Time'].NodeValue := Value;
end;

function TXMLSubject_AreaPropsType.Get_Is_Locked: WideString;
begin
  Result := ChildNodes['Is_Locked'].Text;
end;

procedure TXMLSubject_AreaPropsType.Set_Is_Locked(Value: WideString);
begin
  ChildNodes['Is_Locked'].NodeValue := Value;
end;

function TXMLSubject_AreaPropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

function TXMLSubject_AreaPropsType.Get_Filter_Dangling_Rels_from_Schema_Gen: WideString;
begin
  Result := ChildNodes['Filter_Dangling_Rels_from_Schema_Gen'].Text;
end;

procedure TXMLSubject_AreaPropsType.Set_Filter_Dangling_Rels_from_Schema_Gen(Value: WideString);
begin
  ChildNodes['Filter_Dangling_Rels_from_Schema_Gen'].NodeValue := Value;
end;

{ TXMLNameType }

function TXMLNameType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLNameType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLReferenced_EntitiesType }

function TXMLReferenced_EntitiesType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLReferenced_EntitiesType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLReferenced_EntitiesTypeList }

function TXMLReferenced_EntitiesTypeList.Add: IXMLReferenced_EntitiesType;
begin
  Result := AddItem(-1) as IXMLReferenced_EntitiesType;
end;

function TXMLReferenced_EntitiesTypeList.Insert(const Index: Integer): IXMLReferenced_EntitiesType;
begin
  Result := AddItem(Index) as IXMLReferenced_EntitiesType;
end;
function TXMLReferenced_EntitiesTypeList.Get_Item(Index: Integer): IXMLReferenced_EntitiesType;
begin
  Result := List[Index] as IXMLReferenced_EntitiesType;
end;

{ TXMLReferenced_RelationshipsType }

function TXMLReferenced_RelationshipsType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLReferenced_RelationshipsType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLReferenced_RelationshipsTypeList }

function TXMLReferenced_RelationshipsTypeList.Add: IXMLReferenced_RelationshipsType;
begin
  Result := AddItem(-1) as IXMLReferenced_RelationshipsType;
end;

function TXMLReferenced_RelationshipsTypeList.Insert(const Index: Integer): IXMLReferenced_RelationshipsType;
begin
  Result := AddItem(Index) as IXMLReferenced_RelationshipsType;
end;
function TXMLReferenced_RelationshipsTypeList.Get_Item(Index: Integer): IXMLReferenced_RelationshipsType;
begin
  Result := List[Index] as IXMLReferenced_RelationshipsType;
end;

{ TXMLStored_Display_GroupsType }

procedure TXMLStored_Display_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Stored_Display', TXMLStored_DisplayType);
  inherited;
end;

function TXMLStored_Display_GroupsType.Get_Stored_Display: IXMLStored_DisplayType;
begin
  Result := ChildNodes['Stored_Display'] as IXMLStored_DisplayType;
end;

{ TXMLStored_DisplayType }

procedure TXMLStored_DisplayType.AfterConstruction;
begin
  RegisterChildNode('Stored_DisplayProps', TXMLStored_DisplayPropsType);
  RegisterChildNode('Drawing_Object_Entity_Groups', TXMLDrawing_Object_Entity_GroupsType);
  RegisterChildNode('Drawing_Object_Relationship_Groups', TXMLDrawing_Object_Relationship_GroupsType);
  inherited;
end;

function TXMLStored_DisplayType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLStored_DisplayType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLStored_DisplayType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLStored_DisplayType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLStored_DisplayType.Get_Stored_DisplayProps: IXMLStored_DisplayPropsType;
begin
  Result := ChildNodes['Stored_DisplayProps'] as IXMLStored_DisplayPropsType;
end;

function TXMLStored_DisplayType.Get_Drawing_Object_Entity_Groups: IXMLDrawing_Object_Entity_GroupsType;
begin
  Result := ChildNodes['Drawing_Object_Entity_Groups'] as IXMLDrawing_Object_Entity_GroupsType;
end;

function TXMLStored_DisplayType.Get_Drawing_Object_Relationship_Groups: IXMLDrawing_Object_Relationship_GroupsType;
begin
  Result := ChildNodes['Drawing_Object_Relationship_Groups'] as IXMLDrawing_Object_Relationship_GroupsType;
end;

{ TXMLStored_DisplayPropsType }

procedure TXMLStored_DisplayPropsType.AfterConstruction;
begin
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLStored_DisplayPropsType.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Rel_Name: Integer;
begin
  Result := ChildNodes['Display_Rel_Name'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Rel_Name(Value: Integer);
begin
  ChildNodes['Display_Rel_Name'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Cardinality: Integer;
begin
  Result := ChildNodes['Display_Cardinality'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Cardinality(Value: Integer);
begin
  ChildNodes['Display_Cardinality'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Not_Null: Integer;
begin
  Result := ChildNodes['Display_Not_Null'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Not_Null(Value: Integer);
begin
  ChildNodes['Display_Not_Null'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Physical_Level: Integer;
begin
  Result := ChildNodes['Display_Physical_Level'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Physical_Level(Value: Integer);
begin
  ChildNodes['Display_Physical_Level'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Datatype: Integer;
begin
  Result := ChildNodes['Display_Datatype'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Datatype(Value: Integer);
begin
  ChildNodes['Display_Datatype'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Rolenames: Integer;
begin
  Result := ChildNodes['Display_Rolenames'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Rolenames(Value: Integer);
begin
  ChildNodes['Display_Rolenames'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Alt_Keys: Integer;
begin
  Result := ChildNodes['Display_Alt_Keys'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Alt_Keys(Value: Integer);
begin
  ChildNodes['Display_Alt_Keys'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Level: Integer;
begin
  Result := ChildNodes['Display_Level'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Level(Value: Integer);
begin
  ChildNodes['Display_Level'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Zoom_Option: Integer;
begin
  Result := ChildNodes['Zoom_Option'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Zoom_Option(Value: Integer);
begin
  ChildNodes['Zoom_Option'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_FK: Integer;
begin
  Result := ChildNodes['Display_FK'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_FK(Value: Integer);
begin
  ChildNodes['Display_FK'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Subcat_Name: Integer;
begin
  Result := ChildNodes['Display_Subcat_Name'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Subcat_Name(Value: Integer);
begin
  ChildNodes['Display_Subcat_Name'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Referantial_Integrity: Integer;
begin
  Result := ChildNodes['Display_Referantial_Integrity'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Referantial_Integrity(Value: Integer);
begin
  ChildNodes['Display_Referantial_Integrity'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Physical_Display_Level: Integer;
begin
  Result := ChildNodes['Physical_Display_Level'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Physical_Display_Level(Value: Integer);
begin
  ChildNodes['Physical_Display_Level'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Physical_Display_FK_Name: Integer;
begin
  Result := ChildNodes['Physical_Display_FK_Name'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Physical_Display_FK_Name(Value: Integer);
begin
  ChildNodes['Physical_Display_FK_Name'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Physical_Display_Cardinality: Integer;
begin
  Result := ChildNodes['Physical_Display_Cardinality'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Physical_Display_Cardinality(Value: Integer);
begin
  ChildNodes['Physical_Display_Cardinality'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Physical_Display_Ref_Integ: Integer;
begin
  Result := ChildNodes['Physical_Display_Ref_Integ'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Physical_Display_Ref_Integ(Value: Integer);
begin
  ChildNodes['Physical_Display_Ref_Integ'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Physical_Display_Alt_Keys: Integer;
begin
  Result := ChildNodes['Physical_Display_Alt_Keys'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Physical_Display_Alt_Keys(Value: Integer);
begin
  ChildNodes['Physical_Display_Alt_Keys'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Disallow_Manual_Rel_Layout: WideString;
begin
  Result := ChildNodes['Disallow_Manual_Rel_Layout'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Disallow_Manual_Rel_Layout(Value: WideString);
begin
  ChildNodes['Disallow_Manual_Rel_Layout'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Dont_Display_Views: WideString;
begin
  Result := ChildNodes['Dont_Display_Views'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Dont_Display_Views(Value: WideString);
begin
  ChildNodes['Dont_Display_Views'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Dont_Display_View_Rels: WideString;
begin
  Result := ChildNodes['Dont_Display_View_Rels'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Dont_Display_View_Rels(Value: WideString);
begin
  ChildNodes['Dont_Display_View_Rels'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_View_Col_Datatype: WideString;
begin
  Result := ChildNodes['Display_View_Col_Datatype'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_View_Col_Datatype(Value: WideString);
begin
  ChildNodes['Display_View_Col_Datatype'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_View_Col_Null_Option: WideString;
begin
  Result := ChildNodes['Display_View_Col_Null_Option'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_View_Col_Null_Option(Value: WideString);
begin
  ChildNodes['Display_View_Col_Null_Option'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Ungenerated: WideString;
begin
  Result := ChildNodes['Display_Ungenerated'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Ungenerated(Value: WideString);
begin
  ChildNodes['Display_Ungenerated'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Show_Migrated_Atts: WideString;
begin
  Result := ChildNodes['Show_Migrated_Atts'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Show_Migrated_Atts(Value: WideString);
begin
  ChildNodes['Show_Migrated_Atts'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Logical_Datatype: WideString;
begin
  Result := ChildNodes['Display_Logical_Datatype'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Logical_Datatype(Value: WideString);
begin
  ChildNodes['Display_Logical_Datatype'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Logical_FK_Deisgnator: WideString;
begin
  Result := ChildNodes['Display_Logical_FK_Deisgnator'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Logical_FK_Deisgnator(Value: WideString);
begin
  ChildNodes['Display_Logical_FK_Deisgnator'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Diagonal_Lines: WideString;
begin
  Result := ChildNodes['Display_Diagonal_Lines'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Diagonal_Lines(Value: WideString);
begin
  ChildNodes['Display_Diagonal_Lines'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Attribute_Icons: WideString;
begin
  Result := ChildNodes['Display_Attribute_Icons'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Attribute_Icons(Value: WideString);
begin
  ChildNodes['Display_Attribute_Icons'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Entity_Icons: WideString;
begin
  Result := ChildNodes['Display_Entity_Icons'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Entity_Icons(Value: WideString);
begin
  ChildNodes['Display_Entity_Icons'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Logical_PK_Designator: WideString;
begin
  Result := ChildNodes['Display_Logical_PK_Designator'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Logical_PK_Designator(Value: WideString);
begin
  ChildNodes['Display_Logical_PK_Designator'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Physical_PK_Designator: WideString;
begin
  Result := ChildNodes['Display_Physical_PK_Designator'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Physical_PK_Designator(Value: WideString);
begin
  ChildNodes['Display_Physical_PK_Designator'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_DW_Icons: WideString;
begin
  Result := ChildNodes['Display_DW_Icons'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_DW_Icons(Value: WideString);
begin
  ChildNodes['Display_DW_Icons'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_View_Col_Expr: WideString;
begin
  Result := ChildNodes['Display_View_Col_Expr'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_View_Col_Expr(Value: WideString);
begin
  ChildNodes['Display_View_Col_Expr'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Hide_Rels: WideString;
begin
  Result := ChildNodes['Hide_Rels'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Hide_Rels(Value: WideString);
begin
  ChildNodes['Hide_Rels'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Danging_Rels: WideString;
begin
  Result := ChildNodes['Display_Danging_Rels'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Danging_Rels(Value: WideString);
begin
  ChildNodes['Display_Danging_Rels'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Disallow_Manual_Entity_Resize: WideString;
begin
  Result := ChildNodes['Disallow_Manual_Entity_Resize'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Disallow_Manual_Entity_Resize(Value: WideString);
begin
  ChildNodes['Disallow_Manual_Entity_Resize'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Shadow: WideString;
begin
  Result := ChildNodes['Shadow'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Shadow(Value: WideString);
begin
  ChildNodes['Shadow'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Shadow_X: Integer;
begin
  Result := ChildNodes['Shadow_X'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Shadow_X(Value: Integer);
begin
  ChildNodes['Shadow_X'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Shadow_Y: Integer;
begin
  Result := ChildNodes['Shadow_Y'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Shadow_Y(Value: Integer);
begin
  ChildNodes['Shadow_Y'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_V_Scroll_Pos: Integer;
begin
  Result := ChildNodes['V_Scroll_Pos'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_V_Scroll_Pos(Value: Integer);
begin
  ChildNodes['V_Scroll_Pos'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_H_Scroll_Pos: Integer;
begin
  Result := ChildNodes['H_Scroll_Pos'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_H_Scroll_Pos(Value: Integer);
begin
  ChildNodes['H_Scroll_Pos'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Modified_Time: Integer;
begin
  Result := ChildNodes['Modified_Time'].NodeValue;
end;

procedure TXMLStored_DisplayPropsType.Set_Modified_Time(Value: Integer);
begin
  ChildNodes['Modified_Time'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Is_Locked: WideString;
begin
  Result := ChildNodes['Is_Locked'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Is_Locked(Value: WideString);
begin
  ChildNodes['Is_Locked'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Display_Logical_Domains: WideString;
begin
  Result := ChildNodes['Display_Logical_Domains'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Display_Logical_Domains(Value: WideString);
begin
  ChildNodes['Display_Logical_Domains'].NodeValue := Value;
end;

function TXMLStored_DisplayPropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

function TXMLStored_DisplayPropsType.Get_Print_Info: WideString;
begin
  Result := ChildNodes['Print_Info'].Text;
end;

procedure TXMLStored_DisplayPropsType.Set_Print_Info(Value: WideString);
begin
  ChildNodes['Print_Info'].NodeValue := Value;
end;

{ TXMLDrawing_Object_Entity_GroupsType }

procedure TXMLDrawing_Object_Entity_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Drawing_Object_Entity', TXMLDrawing_Object_EntityType);
  ItemTag := 'Drawing_Object_Entity';
  ItemInterface := IXMLDrawing_Object_EntityType;
  inherited;
end;

function TXMLDrawing_Object_Entity_GroupsType.Get_Drawing_Object_Entity(Index: Integer): IXMLDrawing_Object_EntityType;
begin
  Result := List[Index] as IXMLDrawing_Object_EntityType;
end;

function TXMLDrawing_Object_Entity_GroupsType.Add: IXMLDrawing_Object_EntityType;
begin
  Result := AddItem(-1) as IXMLDrawing_Object_EntityType;
end;

function TXMLDrawing_Object_Entity_GroupsType.Insert(const Index: Integer): IXMLDrawing_Object_EntityType;
begin
  Result := AddItem(Index) as IXMLDrawing_Object_EntityType;
end;

{ TXMLDrawing_Object_EntityType }

procedure TXMLDrawing_Object_EntityType.AfterConstruction;
begin
  RegisterChildNode('Drawing_Object_EntityProps', TXMLDrawing_Object_EntityPropsType);
  inherited;
end;

function TXMLDrawing_Object_EntityType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDrawing_Object_EntityType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLDrawing_Object_EntityType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLDrawing_Object_EntityType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLDrawing_Object_EntityType.Get_Drawing_Object_EntityProps: IXMLDrawing_Object_EntityPropsType;
begin
  Result := ChildNodes['Drawing_Object_EntityProps'] as IXMLDrawing_Object_EntityPropsType;
end;

{ TXMLDrawing_Object_EntityPropsType }

function TXMLDrawing_Object_EntityPropsType.Get_DO_Text: WideString;
begin
  Result := ChildNodes['DO_Text'].Text;
end;

procedure TXMLDrawing_Object_EntityPropsType.Set_DO_Text(Value: WideString);
begin
  ChildNodes['DO_Text'].NodeValue := Value;
end;

function TXMLDrawing_Object_EntityPropsType.Get_DO_Location: WideString;
begin
  Result := ChildNodes['DO_Location'].Text;
end;

procedure TXMLDrawing_Object_EntityPropsType.Set_DO_Location(Value: WideString);
begin
  ChildNodes['DO_Location'].NodeValue := Value;
end;

function TXMLDrawing_Object_EntityPropsType.Get_DO_Reference_Object: WideString;
begin
  Result := ChildNodes['DO_Reference_Object'].Text;
end;

procedure TXMLDrawing_Object_EntityPropsType.Set_DO_Reference_Object(Value: WideString);
begin
  ChildNodes['DO_Reference_Object'].NodeValue := Value;
end;

function TXMLDrawing_Object_EntityPropsType.Get_DO_Entity_Width_AutoResizeable: WideString;
begin
  Result := ChildNodes['DO_Entity_Width_AutoResizeable'].Text;
end;

procedure TXMLDrawing_Object_EntityPropsType.Set_DO_Entity_Width_AutoResizeable(Value: WideString);
begin
  ChildNodes['DO_Entity_Width_AutoResizeable'].NodeValue := Value;
end;

function TXMLDrawing_Object_EntityPropsType.Get_DO_Entity_Height_AutoResizeable: WideString;
begin
  Result := ChildNodes['DO_Entity_Height_AutoResizeable'].Text;
end;

procedure TXMLDrawing_Object_EntityPropsType.Set_DO_Entity_Height_AutoResizeable(Value: WideString);
begin
  ChildNodes['DO_Entity_Height_AutoResizeable'].NodeValue := Value;
end;

{ TXMLDrawing_Object_Relationship_GroupsType }

procedure TXMLDrawing_Object_Relationship_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Drawing_Object_Relationship', TXMLDrawing_Object_RelationshipType);
  inherited;
end;

function TXMLDrawing_Object_Relationship_GroupsType.Get_Drawing_Object_Relationship: IXMLDrawing_Object_RelationshipType;
begin
  Result := ChildNodes['Drawing_Object_Relationship'] as IXMLDrawing_Object_RelationshipType;
end;

{ TXMLDrawing_Object_RelationshipType }

procedure TXMLDrawing_Object_RelationshipType.AfterConstruction;
begin
  RegisterChildNode('Drawing_Object_RelationshipProps', TXMLDrawing_Object_RelationshipPropsType);
  inherited;
end;

function TXMLDrawing_Object_RelationshipType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDrawing_Object_RelationshipType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLDrawing_Object_RelationshipType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLDrawing_Object_RelationshipType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLDrawing_Object_RelationshipType.Get_Drawing_Object_RelationshipProps: IXMLDrawing_Object_RelationshipPropsType;
begin
  Result := ChildNodes['Drawing_Object_RelationshipProps'] as IXMLDrawing_Object_RelationshipPropsType;
end;

{ TXMLDrawing_Object_RelationshipPropsType }

procedure TXMLDrawing_Object_RelationshipPropsType.AfterConstruction;
begin
  FDO_Relationship_Path := CreateCollection(TXMLString_List, IXMLNode, 'DO_Relationship_Path') as IXMLString_List;
  inherited;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Text: WideString;
begin
  Result := ChildNodes['DO_Text'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Text(Value: WideString);
begin
  ChildNodes['DO_Text'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Reference_Object: WideString;
begin
  Result := ChildNodes['DO_Reference_Object'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Reference_Object(Value: WideString);
begin
  ChildNodes['DO_Reference_Object'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Relationship_Path: IXMLString_List;
begin
  Result := FDO_Relationship_Path;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_User_Controled_Path: WideString;
begin
  Result := ChildNodes['DO_User_Controled_Path'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_User_Controled_Path(Value: WideString);
begin
  ChildNodes['DO_User_Controled_Path'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Parent_Side_Verb_Phrase: Integer;
begin
  Result := ChildNodes['DO_Parent_Side_Verb_Phrase'].NodeValue;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Parent_Side_Verb_Phrase(Value: Integer);
begin
  ChildNodes['DO_Parent_Side_Verb_Phrase'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Childside_Verb_Phrase: Integer;
begin
  Result := ChildNodes['DO_Childside_Verb_Phrase'].NodeValue;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Childside_Verb_Phrase(Value: Integer);
begin
  ChildNodes['DO_Childside_Verb_Phrase'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Mid_Point_of_Relationship: WideString;
begin
  Result := ChildNodes['DO_Mid_Point_of_Relationship'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Mid_Point_of_Relationship(Value: WideString);
begin
  ChildNodes['DO_Mid_Point_of_Relationship'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Parent_Side_Verb_Phrase_Coordinates: WideString;
begin
  Result := ChildNodes['DO_Parent_Side_Verb_Phrase_Coordinates'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Parent_Side_Verb_Phrase_Coordinates(Value: WideString);
begin
  ChildNodes['DO_Parent_Side_Verb_Phrase_Coordinates'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Child_Side_Verb_Phrase_Coordinates: WideString;
begin
  Result := ChildNodes['DO_Child_Side_Verb_Phrase_Coordinates'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Child_Side_Verb_Phrase_Coordinates(Value: WideString);
begin
  ChildNodes['DO_Child_Side_Verb_Phrase_Coordinates'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Color_Inherited: WideString;
begin
  Result := ChildNodes['DO_Color_Inherited'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Color_Inherited(Value: WideString);
begin
  ChildNodes['DO_Color_Inherited'].NodeValue := Value;
end;

function TXMLDrawing_Object_RelationshipPropsType.Get_DO_Font_Inherited: WideString;
begin
  Result := ChildNodes['DO_Font_Inherited'].Text;
end;

procedure TXMLDrawing_Object_RelationshipPropsType.Set_DO_Font_Inherited(Value: WideString);
begin
  ChildNodes['DO_Font_Inherited'].NodeValue := Value;
end;

{ TXMLRelationship_GroupsType }

procedure TXMLRelationship_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Relationship', TXMLRelationshipType);
  ItemTag := 'Relationship';
  ItemInterface := IXMLRelationshipType;
  inherited;
end;

function TXMLRelationship_GroupsType.Get_Relationship(Index: Integer): IXMLRelationshipType;
begin
  Result := List[Index] as IXMLRelationshipType;
end;

function TXMLRelationship_GroupsType.Add: IXMLRelationshipType;
begin
  Result := AddItem(-1) as IXMLRelationshipType;
end;

function TXMLRelationship_GroupsType.Insert(const Index: Integer): IXMLRelationshipType;
begin
  Result := AddItem(Index) as IXMLRelationshipType;
end;

{ TXMLRelationshipType }

procedure TXMLRelationshipType.AfterConstruction;
begin
  RegisterChildNode('RelationshipProps', TXMLRelationshipPropsType);
  inherited;
end;

function TXMLRelationshipType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLRelationshipType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLRelationshipType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLRelationshipType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLRelationshipType.Get_RelationshipProps: IXMLRelationshipPropsType;
begin
  Result := ChildNodes['RelationshipProps'] as IXMLRelationshipPropsType;
end;

{ TXMLRelationshipPropsType }

function TXMLRelationshipPropsType.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLRelationshipPropsType.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Type_: Integer;
begin
  Result := ChildNodes['Type'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Type_(Value: Integer);
begin
  ChildNodes['Type'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Note: WideString;
begin
  Result := ChildNodes['Note'].Text;
end;

procedure TXMLRelationshipPropsType.Set_Note(Value: WideString);
begin
  ChildNodes['Note'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Cardinality: WideString;
begin
  Result := ChildNodes['Cardinality'].Text;
end;

procedure TXMLRelationshipPropsType.Set_Cardinality(Value: WideString);
begin
  ChildNodes['Cardinality'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_No_Nulls: Integer;
begin
  Result := ChildNodes['Relationship_No_Nulls'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_No_Nulls(Value: Integer);
begin
  ChildNodes['Relationship_No_Nulls'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Sequence: Integer;
begin
  Result := ChildNodes['Relationship_Sequence'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Sequence(Value: Integer);
begin
  ChildNodes['Relationship_Sequence'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Parent_Update_Rule: Integer;
begin
  Result := ChildNodes['Relationship_Parent_Update_Rule'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Parent_Update_Rule(Value: Integer);
begin
  ChildNodes['Relationship_Parent_Update_Rule'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Parent_Delete_Rule: Integer;
begin
  Result := ChildNodes['Relationship_Parent_Delete_Rule'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Parent_Delete_Rule(Value: Integer);
begin
  ChildNodes['Relationship_Parent_Delete_Rule'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Parent_Insert_Rule: Integer;
begin
  Result := ChildNodes['Relationship_Parent_Insert_Rule'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Parent_Insert_Rule(Value: Integer);
begin
  ChildNodes['Relationship_Parent_Insert_Rule'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Child_Update_Rule: Integer;
begin
  Result := ChildNodes['Relationship_Child_Update_Rule'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Child_Update_Rule(Value: Integer);
begin
  ChildNodes['Relationship_Child_Update_Rule'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Child_Delete_Rule: Integer;
begin
  Result := ChildNodes['Relationship_Child_Delete_Rule'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Child_Delete_Rule(Value: Integer);
begin
  ChildNodes['Relationship_Child_Delete_Rule'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Child_Insert_Rule: Integer;
begin
  Result := ChildNodes['Relationship_Child_Insert_Rule'].NodeValue;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Child_Insert_Rule(Value: Integer);
begin
  ChildNodes['Relationship_Child_Insert_Rule'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Physical_Only: WideString;
begin
  Result := ChildNodes['Physical_Only'].Text;
end;

procedure TXMLRelationshipPropsType.Set_Physical_Only(Value: WideString);
begin
  ChildNodes['Physical_Only'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Parent_Entity: WideString;
begin
  Result := ChildNodes['Relationship_Parent_Entity'].Text;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Parent_Entity(Value: WideString);
begin
  ChildNodes['Relationship_Parent_Entity'].NodeValue := Value;
end;

function TXMLRelationshipPropsType.Get_Relationship_Child_Entity: WideString;
begin
  Result := ChildNodes['Relationship_Child_Entity'].Text;
end;

procedure TXMLRelationshipPropsType.Set_Relationship_Child_Entity(Value: WideString);
begin
  ChildNodes['Relationship_Child_Entity'].NodeValue := Value;
end;

{ TXMLDefault_Value_GroupsType }

procedure TXMLDefault_Value_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Default_Value', TXMLDefault_ValueType);
  ItemTag := 'Default_Value';
  ItemInterface := IXMLDefault_ValueType;
  inherited;
end;

function TXMLDefault_Value_GroupsType.Get_Default_Value(Index: Integer): IXMLDefault_ValueType;
begin
  Result := List[Index] as IXMLDefault_ValueType;
end;

function TXMLDefault_Value_GroupsType.Add: IXMLDefault_ValueType;
begin
  Result := AddItem(-1) as IXMLDefault_ValueType;
end;

function TXMLDefault_Value_GroupsType.Insert(const Index: Integer): IXMLDefault_ValueType;
begin
  Result := AddItem(Index) as IXMLDefault_ValueType;
end;

{ TXMLDefault_ValueType }

procedure TXMLDefault_ValueType.AfterConstruction;
begin
  RegisterChildNode('Default_ValueProps', TXMLDefault_ValuePropsType);
  inherited;
end;

function TXMLDefault_ValueType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDefault_ValueType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLDefault_ValueType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLDefault_ValueType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLDefault_ValueType.Get_Default_ValueProps: IXMLDefault_ValuePropsType;
begin
  Result := ChildNodes['Default_ValueProps'] as IXMLDefault_ValuePropsType;
end;

{ TXMLDefault_ValuePropsType }

procedure TXMLDefault_ValuePropsType.AfterConstruction;
begin
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLDefault_ValuePropsType.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLDefault_ValuePropsType.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLDefault_ValuePropsType.Get_Usage_Count: Integer;
begin
  Result := ChildNodes['Usage_Count'].NodeValue;
end;

procedure TXMLDefault_ValuePropsType.Set_Usage_Count(Value: Integer);
begin
  ChildNodes['Usage_Count'].NodeValue := Value;
end;

function TXMLDefault_ValuePropsType.Get_Server_Value: WideString;
begin
  Result := ChildNodes['Server_Value'].Text;
end;

procedure TXMLDefault_ValuePropsType.Set_Server_Value(Value: WideString);
begin
  ChildNodes['Server_Value'].NodeValue := Value;
end;

function TXMLDefault_ValuePropsType.Get_Physical_Only: WideString;
begin
  Result := ChildNodes['Physical_Only'].Text;
end;

procedure TXMLDefault_ValuePropsType.Set_Physical_Only(Value: WideString);
begin
  ChildNodes['Physical_Only'].NodeValue := Value;
end;

function TXMLDefault_ValuePropsType.Get_Physical_Name: WideString;
begin
  Result := ChildNodes['Physical_Name'].Text;
end;

procedure TXMLDefault_ValuePropsType.Set_Physical_Name(Value: WideString);
begin
  ChildNodes['Physical_Name'].NodeValue := Value;
end;

function TXMLDefault_ValuePropsType.Get_LogicalDefault_Value: WideString;
begin
  Result := ChildNodes['LogicalDefault_Value'].Text;
end;

procedure TXMLDefault_ValuePropsType.Set_LogicalDefault_Value(Value: WideString);
begin
  ChildNodes['LogicalDefault_Value'].NodeValue := Value;
end;

function TXMLDefault_ValuePropsType.Get_Is_Builtin: WideString;
begin
  Result := ChildNodes['Is_Builtin'].Text;
end;

procedure TXMLDefault_ValuePropsType.Set_Is_Builtin(Value: WideString);
begin
  ChildNodes['Is_Builtin'].NodeValue := Value;
end;

function TXMLDefault_ValuePropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

function TXMLDefault_ValuePropsType.Get_System_Generated: WideString;
begin
  Result := ChildNodes['System_Generated'].Text;
end;

procedure TXMLDefault_ValuePropsType.Set_System_Generated(Value: WideString);
begin
  ChildNodes['System_Generated'].NodeValue := Value;
end;

{ TXMLTrigger_Template_GroupsType }

procedure TXMLTrigger_Template_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Trigger_Template', TXMLTrigger_TemplateType);
  ItemTag := 'Trigger_Template';
  ItemInterface := IXMLTrigger_TemplateType;
  inherited;
end;

function TXMLTrigger_Template_GroupsType.Get_Trigger_Template(Index: Integer): IXMLTrigger_TemplateType;
begin
  Result := List[Index] as IXMLTrigger_TemplateType;
end;

function TXMLTrigger_Template_GroupsType.Add: IXMLTrigger_TemplateType;
begin
  Result := AddItem(-1) as IXMLTrigger_TemplateType;
end;

function TXMLTrigger_Template_GroupsType.Insert(const Index: Integer): IXMLTrigger_TemplateType;
begin
  Result := AddItem(Index) as IXMLTrigger_TemplateType;
end;

{ TXMLTrigger_TemplateType }

procedure TXMLTrigger_TemplateType.AfterConstruction;
begin
  RegisterChildNode('Trigger_TemplateProps', TXMLTrigger_TemplatePropsType);
  inherited;
end;

function TXMLTrigger_TemplateType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLTrigger_TemplateType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLTrigger_TemplateType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLTrigger_TemplateType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLTrigger_TemplateType.Get_Trigger_TemplateProps: IXMLTrigger_TemplatePropsType;
begin
  Result := ChildNodes['Trigger_TemplateProps'] as IXMLTrigger_TemplatePropsType;
end;

{ TXMLTrigger_TemplatePropsType }

procedure TXMLTrigger_TemplatePropsType.AfterConstruction;
begin
  RegisterChildNode('Name', TXMLNameType);
  RegisterChildNode('Type', TXMLType_);
  RegisterChildNode('Template_Code', TXMLTemplate_CodeType);
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLTrigger_TemplatePropsType.Get_Name: IXMLNameType;
begin
  Result := ChildNodes['Name'] as IXMLNameType;
end;

function TXMLTrigger_TemplatePropsType.Get_Type_: IXMLType_;
begin
  Result := ChildNodes['Type'] as IXMLType_;
end;

function TXMLTrigger_TemplatePropsType.Get_Template_Code: IXMLTemplate_CodeType;
begin
  Result := ChildNodes['Template_Code'] as IXMLTemplate_CodeType;
end;

function TXMLTrigger_TemplatePropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

{ TXMLTemplate_CodeType }

function TXMLTemplate_CodeType.Get_Space: WideString;
begin
  Result := AttributeNodes['space'].Text;
end;

procedure TXMLTemplate_CodeType.Set_Space(Value: WideString);
begin
  SetAttribute('space', Value);
end;

{ TXMLDefault_Trigger_Template_GroupsType }

procedure TXMLDefault_Trigger_Template_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Default_Trigger_Template', TXMLDefault_Trigger_TemplateType);
  ItemTag := 'Default_Trigger_Template';
  ItemInterface := IXMLDefault_Trigger_TemplateType;
  inherited;
end;

function TXMLDefault_Trigger_Template_GroupsType.Get_Default_Trigger_Template(Index: Integer): IXMLDefault_Trigger_TemplateType;
begin
  Result := List[Index] as IXMLDefault_Trigger_TemplateType;
end;

function TXMLDefault_Trigger_Template_GroupsType.Add: IXMLDefault_Trigger_TemplateType;
begin
  Result := AddItem(-1) as IXMLDefault_Trigger_TemplateType;
end;

function TXMLDefault_Trigger_Template_GroupsType.Insert(const Index: Integer): IXMLDefault_Trigger_TemplateType;
begin
  Result := AddItem(Index) as IXMLDefault_Trigger_TemplateType;
end;

{ TXMLDefault_Trigger_TemplateType }

procedure TXMLDefault_Trigger_TemplateType.AfterConstruction;
begin
  RegisterChildNode('Default_Trigger_TemplateProps', TXMLDefault_Trigger_TemplatePropsType);
  inherited;
end;

function TXMLDefault_Trigger_TemplateType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDefault_Trigger_TemplateType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLDefault_Trigger_TemplateType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLDefault_Trigger_TemplateType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLDefault_Trigger_TemplateType.Get_Default_Trigger_TemplateProps: IXMLDefault_Trigger_TemplatePropsType;
begin
  Result := ChildNodes['Default_Trigger_TemplateProps'] as IXMLDefault_Trigger_TemplatePropsType;
end;

{ TXMLDefault_Trigger_TemplatePropsType }

procedure TXMLDefault_Trigger_TemplatePropsType.AfterConstruction;
begin
  RegisterChildNode('Template_Purpose_Text', TXMLTemplate_Purpose_TextType);
  RegisterChildNode('Template_Purpose', TXMLTemplate_PurposeType);
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLDefault_Trigger_TemplatePropsType.Get_Trigger_Template_Ref: WideString;
begin
  Result := ChildNodes['Trigger_Template_Ref'].Text;
end;

procedure TXMLDefault_Trigger_TemplatePropsType.Set_Trigger_Template_Ref(Value: WideString);
begin
  ChildNodes['Trigger_Template_Ref'].NodeValue := Value;
end;

function TXMLDefault_Trigger_TemplatePropsType.Get_Template_Purpose_Text: IXMLTemplate_Purpose_TextType;
begin
  Result := ChildNodes['Template_Purpose_Text'] as IXMLTemplate_Purpose_TextType;
end;

function TXMLDefault_Trigger_TemplatePropsType.Get_Template_Purpose: IXMLTemplate_PurposeType;
begin
  Result := ChildNodes['Template_Purpose'] as IXMLTemplate_PurposeType;
end;

function TXMLDefault_Trigger_TemplatePropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

{ TXMLTemplate_Purpose_TextType }

function TXMLTemplate_Purpose_TextType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLTemplate_Purpose_TextType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLTemplate_PurposeType }

function TXMLTemplate_PurposeType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLTemplate_PurposeType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLName_Mapping_GroupsType }

procedure TXMLName_Mapping_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Name_Mapping', TXMLName_MappingType);
  ItemTag := 'Name_Mapping';
  ItemInterface := IXMLName_MappingType;
  inherited;
end;

function TXMLName_Mapping_GroupsType.Get_Name_Mapping(Index: Integer): IXMLName_MappingType;
begin
  Result := List[Index] as IXMLName_MappingType;
end;

function TXMLName_Mapping_GroupsType.Add: IXMLName_MappingType;
begin
  Result := AddItem(-1) as IXMLName_MappingType;
end;

function TXMLName_Mapping_GroupsType.Insert(const Index: Integer): IXMLName_MappingType;
begin
  Result := AddItem(Index) as IXMLName_MappingType;
end;

{ TXMLName_MappingType }

procedure TXMLName_MappingType.AfterConstruction;
begin
  RegisterChildNode('Name_MappingProps', TXMLName_MappingPropsType);
  inherited;
end;

function TXMLName_MappingType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLName_MappingType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLName_MappingType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLName_MappingType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLName_MappingType.Get_Name_MappingProps: IXMLName_MappingPropsType;
begin
  Result := ChildNodes['Name_MappingProps'] as IXMLName_MappingPropsType;
end;

{ TXMLName_MappingPropsType }

procedure TXMLName_MappingPropsType.AfterConstruction;
begin
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  inherited;
end;

function TXMLName_MappingPropsType.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLName_MappingPropsType.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLName_MappingPropsType.Get_Name_Mapping_Obj_Type: Integer;
begin
  Result := ChildNodes['Name_Mapping_Obj_Type'].NodeValue;
end;

procedure TXMLName_MappingPropsType.Set_Name_Mapping_Obj_Type(Value: Integer);
begin
  ChildNodes['Name_Mapping_Obj_Type'].NodeValue := Value;
end;

function TXMLName_MappingPropsType.Get_Name_Mapping_Use_Glossary: WideString;
begin
  Result := ChildNodes['Name_Mapping_Use_Glossary'].Text;
end;

procedure TXMLName_MappingPropsType.Set_Name_Mapping_Use_Glossary(Value: WideString);
begin
  ChildNodes['Name_Mapping_Use_Glossary'].NodeValue := Value;
end;

function TXMLName_MappingPropsType.Get_Name_Mapping_Alternate_Abbrev: WideString;
begin
  Result := ChildNodes['Name_Mapping_Alternate_Abbrev'].Text;
end;

procedure TXMLName_MappingPropsType.Set_Name_Mapping_Alternate_Abbrev(Value: WideString);
begin
  ChildNodes['Name_Mapping_Alternate_Abbrev'].NodeValue := Value;
end;

function TXMLName_MappingPropsType.Get_Name_Mapping_Macro_Text: WideString;
begin
  Result := ChildNodes['Name_Mapping_Macro_Text'].Text;
end;

procedure TXMLName_MappingPropsType.Set_Name_Mapping_Macro_Text(Value: WideString);
begin
  ChildNodes['Name_Mapping_Macro_Text'].NodeValue := Value;
end;

function TXMLName_MappingPropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

{ TXMLNaming_Options_GroupsType }

procedure TXMLNaming_Options_GroupsType.AfterConstruction;
begin
  RegisterChildNode('Naming_Options', TXMLNaming_OptionsType);
  ItemTag := 'Naming_Options';
  ItemInterface := IXMLNaming_OptionsType;
  inherited;
end;

function TXMLNaming_Options_GroupsType.Get_Naming_Options(Index: Integer): IXMLNaming_OptionsType;
begin
  Result := List[Index] as IXMLNaming_OptionsType;
end;

function TXMLNaming_Options_GroupsType.Add: IXMLNaming_OptionsType;
begin
  Result := AddItem(-1) as IXMLNaming_OptionsType;
end;

function TXMLNaming_Options_GroupsType.Insert(const Index: Integer): IXMLNaming_OptionsType;
begin
  Result := AddItem(Index) as IXMLNaming_OptionsType;
end;

{ TXMLNaming_OptionsType }

procedure TXMLNaming_OptionsType.AfterConstruction;
begin
  RegisterChildNode('Naming_OptionsProps', TXMLNaming_OptionsPropsType);
  inherited;
end;

function TXMLNaming_OptionsType.Get_Id: WideString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLNaming_OptionsType.Set_Id(Value: WideString);
begin
  SetAttribute('id', Value);
end;

function TXMLNaming_OptionsType.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLNaming_OptionsType.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLNaming_OptionsType.Get_Naming_OptionsProps: IXMLNaming_OptionsPropsType;
begin
  Result := ChildNodes['Naming_OptionsProps'] as IXMLNaming_OptionsPropsType;
end;

{ TXMLNaming_OptionsPropsType }

procedure TXMLNaming_OptionsPropsType.AfterConstruction;
begin
  RegisterChildNode('Name', TXMLNameType);
  RegisterChildNode('Physical_Only', TXMLPhysical_OnlyType);
  RegisterChildNode('Default_Object_Alternative_Id', TXMLDefault_Object_Alternative_IdType);
  RegisterChildNode('Logical_Only', TXMLLogical_OnlyType);
  inherited;
end;

function TXMLNaming_OptionsPropsType.Get_Name: IXMLNameType;
begin
  Result := ChildNodes['Name'] as IXMLNameType;
end;

function TXMLNaming_OptionsPropsType.Get_Physical_Only: IXMLPhysical_OnlyType;
begin
  Result := ChildNodes['Physical_Only'] as IXMLPhysical_OnlyType;
end;

function TXMLNaming_OptionsPropsType.Get_Name_Mapping_Obj_Type: Integer;
begin
  Result := ChildNodes['Name_Mapping_Obj_Type'].NodeValue;
end;

procedure TXMLNaming_OptionsPropsType.Set_Name_Mapping_Obj_Type(Value: Integer);
begin
  ChildNodes['Name_Mapping_Obj_Type'].NodeValue := Value;
end;

function TXMLNaming_OptionsPropsType.Get_Default_Object_Alternative_Id: IXMLDefault_Object_Alternative_IdType;
begin
  Result := ChildNodes['Default_Object_Alternative_Id'] as IXMLDefault_Object_Alternative_IdType;
end;

function TXMLNaming_OptionsPropsType.Get_Logical_Only: IXMLLogical_OnlyType;
begin
  Result := ChildNodes['Logical_Only'] as IXMLLogical_OnlyType;
end;

{ TXMLPhysical_OnlyType }

function TXMLPhysical_OnlyType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLPhysical_OnlyType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLLogical_OnlyType }

function TXMLLogical_OnlyType.Get_RO: WideString;
begin
  Result := AttributeNodes['RO'].Text;
end;

procedure TXMLLogical_OnlyType.Set_RO(Value: WideString);
begin
  SetAttribute('RO', Value);
end;

{ TXMLString_List }

function TXMLString_List.Add(const Value: WideString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := Value;
end;

function TXMLString_List.Insert(const Index: Integer; const Value: WideString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := Value;
end;
function TXMLString_List.Get_Item(Index: Integer): WideString;
begin
  Result := List[Index].NodeValue;
end;

end.
