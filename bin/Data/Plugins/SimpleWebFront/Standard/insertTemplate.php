<?php
//| This file is part of the SimpleWebFront-DBDesigner4-Plugin.
// Copyright (C) 2003 Bayer Ulrich
//
// This file is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------
//|
//|  File:               insertTemplate.php
//|  Author:             Ulrich Bayer
//|  CVS:                $Id: insertTemplate.php,v 1.1.1.1 2003/08/20 15:46:24 fabforce_ulli Exp $
//|  Description: 
//|    This is the template-file for all generated insert-
//|    files. The generated insert-file allows a user
//|    to insert a data tuple into the table.
//|
                

//include common function definitons
include "commonFunctions.php";

//the following include-file connects to the chosen database
/*[[INCLUDE]]*/;
?>

<?php
//we define that here and not in commonFunctions.php since 
//we need to use the magic variable__FILE__
function handle_sql_error($lineNo, $sqlStatement) {
    $filename = __FILE__;
    print "Error in $filename, Line number $lineNo!<br>";
    print "The following SQL-statement has caused the error:<br>";
    print "<b>$sqlStatement</b><br>";
    print "<br>The sql server has returned this error message:<br>";
    $eMsg = mysql_error();
    print $eMsg;
    die(1);
}
?>


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
       "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">
  <meta http-equiv="Content-Script-Type" content="text/javascript">
  <meta http-equiv="Content-Style-Type" content="text/css">
  <meta http-equiv="expires" content="0">
  <meta name="robots" content="noindex">
  <link rel="StyleSheet" href="incs/document.css" type="text/css" media="screen">
  <script type="text/javascript" src="incs/dlg.js"></script> 
  <title>Insert-Window</title>
</head>

<body style="margin-left:5px;margin-top:5px;">
<?php

    //for an explanation of these variables see gridTemplate.php
    //or the developer documentation

    $mainTablename = /*[[TABLENAME]]*/;                  
    $mainTableDataTypes = /*[[DATATYPES]]*/;             
    $mainTableDataTypeParams = /*[[DATATYPE_PARAMS]]*/;
    $mainTableDefaultValue = /*[[MAIN_DEFAULT_VALUE]]*/;
    $mainTableNotNull = /*[[MAIN_NOTNULL]]*/;
    $mainTableKey = /*[[TABLEKEY]]*/;                    
    $mainTableDispCols = /*[[TABLEDISPCOLS]]*/;         
        
    $joinTablenames = /*[[JOINTABLENAMES]]*/;           
    $joinTables_Key = /*[[JOINTABLES_KEY]]*/;           
    $joinTables_DispCols = /*[[JOINTABLES_DISPCOLS]]*/; 

    $nmTablenames = /*[[NMTABLENAMES]]*/;
    $nmTables_Key = /*[[NMTABLES_KEY]]*/;
    $nmTables_DataTypes = /*[[NMTABLES_DATATYPES]]*/;
    $nmTables_TableDataTypeParams = /*[[NMTABLES_DATATYPE_PARAMS]]*/;
    $nmTables_DispCols = /*[[NMTABLES_DISPCOLS]]*/;
    $connTablenames = /*[[CONNECTIONTABLENAMES]]*/;

    //An array describing the (user-defined) captions of all columns that will shown to the user instead of the db-names 
    //The qualified db column-name is used to index the array
    //example: array('product.Name' => 'Name',...)
    $colCaptions =  /*[[COLUMN_CAPTIONS]]*/;
    
    //An associative array that lists all columns that have a width specified with their respective width-values
    //example: array("testTable1.column1" => 55)
    $colWidths = /*[[COLUMN_WIDTHS]]*/; 

    //This array stores a mapping of qualified column names to the (qualified) column names they reference
    //Consequently the index consists only of qualified foreign keynames; their value denotes the column
    //they are refering to;
    $keyMapping= /*[[KEY_MAPPING]]*/;
    
    $grid_filename = /*[[FILENAME_GRID]]*/;
    
    // array("table.column => type,...);
    //type is one of (main,join,nm);
    $colOrder = /*[[COLUMNS_ORDER]]*/;
?>
        
<!-- show the form -->
        
<?php
$sql_filename = substr($grid_filename,0,strlen($grid_filename)-4) . '_sql.php';  
echo "<form name='InsertForm' action='{$sql_filename}?mode=insert' target='Status_section' method='POST'>";

//print the the form
echo "<fieldset><legend>$mainTablename</legend>";
echo "<table>";

//For every column that is selected for display
//print an edit field where the user can enter a value for this column
foreach ($colOrder as $qualCol => $type) {
    
    $fieldname = Dequalify($qualCol);
    if ($type == "main") {
    //foreach ($mainTableDispCols as $fieldname) {
        echo "<tr>"; 
        $tmp = "$mainTablename.$fieldname";
        echo "<td>$colCaptions[$tmp]:</td>";
        
        //For Enums we display a drop-down list
        if ($mainTableDataTypes[$fieldname]== "ENUM" ) {
            if (arrayKeyExists($tmp,$colWidths))
                echo "<td><select name=\"colValues[$fieldname]\" size='1' style=\"width:{$colWidths[$tmp]}px; overflow:hidden;\">";
            else
                echo "<td><select name=\"colValues[$fieldname]\" size='1'>";
    
            //is the null-value allowed?
            if (! $mainTableNotNull[$fieldname])
                echo "<option value=''></option>\n"; 
                    
            foreach($mainTableDataTypeParams[$fieldname] as $dparam)
                if ($mainTableDefaultValue[$fieldname] == $dparam) //preselect the default value (if one is specified)
                    echo "<option selected value='$dparam'>$dparam</option>\n";
                else
                    echo "<option value='$dparam'>$dparam</option>\n";       
                     
            echo "</select></td></tr>";
    
        //For Booleans we display a drop-down list too 
        } else if ($mainTableDataTypes[$fieldname]== "BOOL" ) {
            if (arrayKeyExists($tmp,$colWidths))
                echo "<td><select name=\"colValues[$fieldname]\" size='1' style=\"width:{$colWidths[$tmp]}px; overflow:hidden;\">";
            else
                echo "<td><select name=\"colValues[$fieldname]\" size='1'>";
                    
            //is the null-value allowed?
            if (! $mainTableNotNull[$fieldname])
                echo "<option value=''></option>\n"; 
    
            if ($mainTableDefaultValue[$fieldname] == "FALSE")
                echo "<option selected value='0'>0</option>\n";
            else
                echo "<option value='0'>0</option>\n";
                
            if ($mainTableDefaultValue[$fieldname] == "TRUE")            
                echo "<option selected value='1'>1</option>\n";
            else
                echo "<option value='1'>1</option>\n";
                      
            echo "</select></td></tr>"; 
        
        //in the normal case just display an edit-box
        } else {
            $value = $mainTableDefaultValue[$fieldname];
            
            if (arrayKeyExists($tmp,$colWidths))
                echo "<td><input type='text' name=\"colValues[$fieldname]\" value='$value' style=\"width:{$colWidths[$tmp]}px; overflow:hidden;\"></td>";
            else
                echo "<td><input type='text' name=\"colValues[$fieldname]\" value='$value'></td>";
            echo "</tr>";
        }
    } else if ($type == "join") {
    //foreach ($joinTablenames as $joinTablename) {
        
        //define some basic variables
        $joinTablename = GetTablename($qualCol);
        $joinTableKey = $joinTables_Key[$joinTablename];
        $joinTableDispCols = $joinTables_DispCols[$joinTablename];
    
        //prepare the next sql-query 
    
        //select all fields marked for display + the key + the mainTableKey
        $selectClause = array_unique( array_merge($joinTableKey,$joinTableDispCols) );
        $selectClause = implode("," , $selectClause);
    
        //$query
        $query = "SELECT $selectClause FROM $joinTablename";
            
        //query the db
        $result = mysql_query($query) or handle_sql_error(__LINE__, $query);
        
        $qual_name= "$joinTablename.$joinTableKey[0]";
        echo "<tr><td>$colCaptions[$qual_name]:</td><td>";
        
        //the drop-down box for selecting to what foreign tuple we are attached
        if (arrayKeyExists($qual_name,$colWidths))
            echo "<select name='colValues[joinTableKeySelector_$joinTablename]' size='1' style=\"width:{$colWidths[$qual_name]}px; overflow:hidden;\">";
        else
            echo "<select name='colValues[joinTableKeySelector_$joinTablename]' size='1' >";
            
        //print all options
        while ($row = mysql_fetch_array($result, MYSQL_ASSOC)) {
            
            //find out the internal value of the option-field first
            $keyValue = ""; //The ordering of $keyValue is important! it must follow $joinTableKey                           
            foreach ($joinTableKey as $keyElement) {
                $tmp = "{$row[$keyElement]}";
                $tmp = bin2hex($tmp);
                $keyValue .= "$tmp,";
            }
            $keyValue = substr($keyValue,0, strlen($keyValue)-1); //delete the last comma 
            
            
           echo "<option value=\"$keyValue\" >";
                
            //print the text of the option-field
            $str = '';
            foreach($row as $fieldname=>$fieldValue) {
                if ( in_array($fieldname, $joinTableDispCols) ) 
                    $str .= "$fieldValue,";
            }
            $str = substr($str, 0, strlen($str)-1); //delete the last comma
            echo $str;   
            echo "</option>";  
        }
       
        echo "</select></td></tr>";
    }      
        
}    

    
echo "</table>";
echo "</fieldset>";


foreach($nmTablenames as $nmTablename) {
    
    //define some basic variables
    $nmTableKey = $nmTables_Key[$nmTablename];
    $nmTableDispCols = $nmTables_DispCols[$nmTablename];
    $connTablename = $connTablenames[$nmTablename];
    
    $qual_name = "$nmTablename.$nmTableKey[0]";
    echo "<br>";
    echo "<fieldset>";
    echo "<legend>$nmTablename</legend>";
    echo "<table>";
    echo "<tr><td>Assigned relations</td></tr>";
    $s='';
    foreach ($nmTableDispCols as $field) {
        $t = "$nmTablename.$field";
        $s .= "{$colCaptions[$t]},";
    }
    $s = substr($s, 0, strlen($s)-1);
    echo "<tr><td class=\"fontS\">$s</td></tr>";

    echo "<input type='hidden' name='assignedTuples[$nmTablename]' value=''>";
    
    //**print the assigned relations(ar) select-elemnt**
    if (arrayKeyExists($qual_name, $colWidths))
        echo "<tr><td><select name='AssignedRelations_$nmTablename' size='5' style=\"width:{$colWidths[$qual_name]}px; overflow:hidden;\">";
    else
        echo "<tr><td><select name='AssignedRelations_$nmTablename' size='5'>";
        

    echo "</select></td></tr>";
    
   
    
    //**print the buttons**
    echo "<tr><td height='3'><img src='images/1ptrans.png'></td><td></td></tr>";
    echo "<tr><td><a href='javascript:void up(\"$nmTablename\");'><img src='images/btnup.png'> </a>";
    echo "<a href='javascript: void down(\"$nmTablename\");'><img src='images/btndown.png'></a</td></tr>";
    
    
    //**print all possible relations(pr)**
    echo "<tr><td>All possible relations</td></tr>";
    $s='';
    foreach ($nmTableDispCols as $field) {
        $t = "$nmTablename.$field";
        $s .= "{$colCaptions[$t]},";
    }
    $s = substr($s, 0, strlen($s)-1);
    echo "<tr><td class=\"fontS\">$s</td></tr>";
    
    if (arrayKeyExists($qual_name, $colWidths))
        echo "<tr><td><select name='PossibleRelations_$nmTablename' size='5' style=\"width:{$colWidths[$qual_name]}px; overflow:hidden;\">";
    else
        echo "<tr><td><select name='PossibleRelations_$nmTablename' size='5'>";
        
    $nmTableCols = array_unique((array_merge($nmTableDispCols,$nmTableKey)));
    $nmTableCols_string = implode(",", Qualify($nmTablename,$nmTableCols));
                              
    $prQuery = "SELECT $nmTableCols_string FROM $nmTablename";     
    
    //finally make the query
    $result = mysql_query($prQuery) or handle_sql_error(__LINE__, $prQuery);
    
    //print the query
    while ($row = mysql_fetch_array($result, MYSQL_ASSOC) ) {
                
        $value = '';
        foreach ($nmTableKey as $k) {
            $value .= "{$row[$k]},";
        } 
        $value = substr($value, 0, strlen($value)-1);
        
        $s='';
        foreach ($nmTableDispCols as $field) {
            $s .= "{$row[$field]},";
        }
        $s = substr($s, 0, strlen($s)-1);
        
        
        echo "<option value='$value'>";
        echo $s;        
        echo "</option>";
        
    }
    echo "</select></td></tr>";
    echo "</table>";
    echo "</fieldset>";
}
?>

<br>
<br>

<div align= "right">
<table border='0'>
<tr>
<td><a href='javascript: void window.close();'><img src="images/btncancel.png"></a></td>
<td><a href='javascript: void processForm();'><img src="images/btnok.png"></a></td>
<td><img src="images/1ptrans.png" width="10"></td>
</tr>
</table>
</div>

</form>



<script type="text/javascript">

//This function moves an entry from the list of all possible relations to the "Assigned-Relations" list
function up(nmTablename) {
    
    var i = 0;
    var name1 = "AssignedRelations_"+nmTablename;
    var name2 = "PossibleRelations_"+nmTablename;
    var assignedRelations;
    var possibleRelations;
    
    //get a pointer to the two lists
    while (i < document.forms[0].elements.length) {
        if (! document.forms[0].elements[i].name) { //the fieldset-tag has no name
            i++;
            continue;
        } else if (document.forms[0].elements[i].name == name1) {
            assignedRelations = document.forms[0].elements[i];
        } else if (document.forms[0].elements[i].name == name2) {
            possibleRelations = document.forms[0].elements[i];
        }
        i++;
    }
     
    //find the selected entry
    var index = possibleRelations.options.selectedIndex;
    if (index == -1)
        return;
    var entry = possibleRelations.options[index];
    
    //add the element to the AssigendRelations selection-field
    var newEntry = new Option(entry.text, entry.value,false, false);
    assignedRelations.options[assignedRelations.options.length] = newEntry;
    
    //delete the element from the PossibleRelations selection-field
    possibleRelations.options[index] = null;
    
    
    possibleRelations.selectedIndex = -1;
}

//This function moves an entry from the "Assigned-Relations" list to the list of all possible relations
function down(nmTablename) {
    
    var i = 0;
    var name1 = "AssignedRelations_"+nmTablename;
    var name2 = "PossibleRelations_"+nmTablename;
    var assignedRelations;
    var possibleRelations;
    
    //get a pointer to the two lists
    while (i < document.forms[0].elements.length) {
        if (! document.forms[0].elements[i].name) { //the fieldset-tag has no name
            i++;
            continue;
        } else if (document.forms[0].elements[i].name == name1) {
            assignedRelations = document.forms[0].elements[i];
        } else if (document.forms[0].elements[i].name == name2) {
            possibleRelations = document.forms[0].elements[i];
        }
        i++;
    }
    
    //find the selected entry
    var index = assignedRelations.options.selectedIndex;
    if (index == -1)
        return;
    var entry = assignedRelations.options[index];
    
    //add the element to the AssigendRelations selection-field
    var newEntry = new Option(entry.text, entry.value,false, false);
    possibleRelations.options[possibleRelations.options.length] = newEntry;
    
    //delete the element from the PossibleRelations selection-field
    assignedRelations.options[index] = null;
    
    
    assignedRelations.selectedIndex = -1;
}

//checks if all fields that may not be null have been filled in
function check() {
    var i = 0;
    var elementName = '';
    
    <?php
        echo "var notNull = new Array();";
        foreach ($mainTableNotNull as $coln=>$bool)
            if ($bool) 
                echo "notNull['$coln'] = true;";
            else
                echo "notNull['$coln'] = false;";
    ?>
    
    //iterate through all elements of the form
    while (i < document.forms[0].elements.length) {
        if (! document.forms[0].elements[i].name) { //we need that check since the fieldset-tag has no name
            i++;
            continue;
        }
        elementName = document.forms[0].elements[i].name;
        if (elementName.indexOf("colValues") == 0) {
            elementName = elementName.slice(10,elementName.length-1);  //delete the prefix colValues[
            
            //we don't check joinTablefields at the moment
            if (elementName.indexOf("joinTableKeySelector_") == 0) {i++; continue; }
            
            if ((notNull[elementName]) && (document.forms[0].elements[i].value == '') ) {
                alert('Please enter a value for '+elementName+'!');
                return false;
            }
        }
        
        
        i++;    
    }
    return true;   
}

//this function is executed when the ok-button is clicked
//For Assigned_Relations we need to take special measures here
//Then we submit the form and close the window afterwards
function processForm() {
    var i = 0;
    var elementName = '';
    
    if (! check()) return;
    
    //forward all values of the maintable-columns
    while (i < document.forms[0].elements.length) {
        if (! document.forms[0].elements[i].name) { //we need that check since the fieldset-tag has no name
            i++;
            continue;
        }
        elementName = document.forms[0].elements[i].name;

        
        if (elementName.indexOf("AssignedRelations_") == 0) {
            var nmTablename= elementName.slice(18,elementName.length);  //delete the prefix
            var j=0;
            var s = "";
	    var tmpStr = '';
            
            while (j < document.forms[0].elements[i].options.length) {
                s+= document.forms[0].elements[i].options[j].value+";";
                j++;
            }
            s = s.slice(0,s.length-1); //delete the last ;
            tmpStr = 'assignedTuples['+nmTablename+']';

	    //now search the appropriate hidden input-field
	    while (j < document.forms[0].elements.length) {
	      if (! document.forms[0].elements[j].name) { //we need that check since the fieldset-tag has no name
		j++;
		continue;
	      }
	      if (document.forms[0].elements[j].name == tmpStr) {
		document.forms[0].elements[j].value = s;
	      }
	      j++;
	    }
        }
        
        i++;    
    }
        
    document.forms[0].submit();
    opener.focus();
    window.close();    
}

            
</script>


</body>
</html>


<?php
// close the db-connection
mysql_close($dbLink);
?>