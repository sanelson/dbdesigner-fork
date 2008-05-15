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
//|  File:               processSQLTemplate.php
//|  Author:             Ulrich Bayer
//|  CVS:                $Id: processSQLTemplate.php,v 1.1.1.1 2003/08/20 15:46:32 fabforce_ulli Exp $
//|  Description: 
//|    This is the template-file for all generated process-
//|    SQL-files. Each generated file is responsible for
//|    processing SQL Delete,Insert and Update requests of
//|    the view it belongs to.
//|     

//include common function definitons
include "commonFunctions.php";

//the include-file connects to the chosen database
/*[[INCLUDE]]*/;
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
</head>


<?php
$mainTablename = /*[[TABLENAME]]*/; 
$mainTableKey = /*[[TABLEKEY]]*/;

$joinTables_Key = /*[[JOINTABLES_KEY]]*/;  

$nmTablenames = /*[[NMTABLENAMES]]*/;
$nmTables_Key = /*[[NMTABLES_KEY]]*/;
$nmTables_DataTypes = /*[[NMTABLES_DATATYPES]]*/;

$connTablenames = /*[[CONNECTIONTABLENAMES]]*/;
$keyMapping= /*[[KEY_MAPPING]]*/;
?>


<body onLoad='parent.Main.location.reload()' style="background-image:url('images/statusbg.png')" >


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

function statusOutput($message)
{
  echo "";
  echo "<table  border='0' align='center'>";
  echo "<tr><td class='fontN'>";
  echo $message;
  echo "</td></tr>";
  echo "</table>";

}

            
//delete a row
if ($HTTP_GET_VARS["mode"]=='del') {
    $values = $HTTP_GET_VARS["keyValues"]; //values is an associative array of the form $values[keyMember1]=value
    $keys = array_keys($values);
    
    $delStatement = "DELETE FROM $mainTablename WHERE ";
    $u=0;
    while ($u < count($values)) {
        $delStatement .= $keys[$u] . '=' . format($values[$keys[$u]],$keys[$u]) . ' AND ';
        $u++;
    }
    
    //delete the last ' AND '
    $delStatement = substr($delStatement,0, strlen($delStatement)-5);
    

    $successful = mysql_query($delStatement) or handle_sql_error(__LINE__, $delStatement);
    
    statusOutput( "Successfully deleted one row!");

//insert a row
} elseif ($HTTP_GET_VARS["mode"]=='insert') {

    //get the values of the record to be inserted
    $colValues = $HTTP_POST_VARS["colValues"];
    
    $attList = "( ";
    $values = "( ";
    foreach ($colValues as $name=>$val) {
        if (strpos($name, "joinTableKeySelector_") === 0) {
            $joinTablename = substr($name, 21);
            $joinTableKey = $joinTables_Key[$joinTablename];
            
            $pieces = explode("," , $val);
            
            $i = 0;
            while($i < count($joinTableKey)) {
                $pieces[$i] = hex2bin($pieces[$i]);
                $attList .= Dequalify(foreignKeyname($mainTablename, "$joinTablename.{$joinTableKey[$i]}")) .",";
                $values .= format($pieces[$i],$joinTableKey[$i]).",";

                $i++;
            }
        } else {
             //if it is a foreignKey
            if (in_array("$mainTablename.$name", array_keys($keyMapping))) {   
                $joinTablename = GetTablename($keyMapping["$mainTablename.$name"]);
                
                //if there is also a drop-down box for choosing this foreignKey we take the value of the box
                if (arrayKeyExists("joinTableKeySelector_$joinTablename", $colValues)) 
                    continue;
            }
            $attList .= "$name,";
            $values .= format($val,$name) .",";
        }
    }
    $attList[strlen($attList)-1] = ")";
    $values[strlen($values)-1] = ")";
                        
    $insertStatement = "INSERT INTO $mainTablename $attList VALUES $values";
    $successful = mysql_query($insertStatement) or handle_sql_error(__LINE__, $insertStatement);
    
    if ( isset($HTTP_POST_VARS["assignedTuples"]) ) {
        
        //we need to find out the value of the key of the tuple we inserted before
        
        $KeyValueGiven = true; //did the user supply the value for the key?
        $lastInsertKey = array(); //used to store the key of the last insert
        
        foreach ($mainTableKey as $keyPart) {
            
            //if the form-window didn't supply a value for the key
            if (  (!isset($colValues[$keyPart])) || ($colValues[$keyPart] == '')) {
                $KeyValueGiven = false;break;
            }
            $lastInsertKey[$keyPart] = $colValues[$keyPart];
        }
        
        if (! $KeyValueGiven) {    //the user supplied no values for the key-attribute(s)
            //this means the key consists of exactly one column that has Auto-increment turned on
            assert(count($mainTableKey) == 1); 
            $lastInsertKey[$mainTableKey[0]] = mysql_insert_id();
        }                       
    }
    
    //for nm-tables only
    foreach ($nmTablenames as $nmTablename) {
        $connTablename = $connTablenames[$nmTablename];
        $nmTableKey = $nmTables_Key[$nmTablename];
        
        //has the user also assigned any nm-tupes?        
        if ( isset($HTTP_POST_VARS["assignedTuples"]) ) {
            
            //now insert all assigned tuples
            $assignedTuples = $HTTP_POST_VARS["assignedTuples"]; //the keys of the assigned tuples
            
            //assignedTuples will look like this: assignedTuples[tablename]=9;2;1
            $assignedTuples = $assignedTuples[$nmTablename];
            $assignedTuples = explode (";", $assignedTuples); // ';' separates the keys of different tuples
            
            foreach ($assignedTuples as $assignedTuple) {
                $pieces = explode (",", $assignedTuple);   // ',' separates the columns that make up the key 
                $i=0;
                $iCols = '(';
                $iValues = '(';
                
                while ($i < count($nmTableKey) ) {
                    $iCols .= Dequalify(foreignKeyname($connTablename,"$nmTablename.$nmTableKey[$i]")) .",";
                    $iValues .= format($pieces[$i], $nmTableKey[$i], $nmTablename) .",";
                    $i++;
                }
                
                foreach ($lastInsertKey as $name => $val) {
                    $iCols .= Dequalify(foreignKeyname($connTablename, "$mainTablename.$name")).",";
                    $iValues .= format($val,$name) . ",";
                }

                
                $iCols[strlen($iCols)-1] = ')';
                $iValues[strlen($iValues)-1] = ')';
                
                $insertStatement = "INSERT INTO $connTablename $iCols VALUES $iValues";
                
                $successful = mysql_query($insertStatement) or handle_sql_error(__LINE__, $insertStatement);
            }
        }

    }   

    statusOutput( "Insert was successful!");
               
//update a row               
} elseif ($HTTP_GET_VARS["mode"]=='update') {

    $tupleKey = $HTTP_POST_VARS["tupleKey"]; //identifies the tuple that should be updated                
    $colValues = $HTTP_POST_VARS["colValues"]; //the new values

    $setClause='';               
    foreach ($colValues as $name=>$val) {
        if (strpos($name, "joinTableKeySelector_") === 0) {
            $joinTablename = substr($name, 21);
            $joinTableKey = $joinTables_Key[$joinTablename];
            $pieces = explode("," , $val);
            
            $i = 0;
            while($i < count($joinTableKey)) {
                $pieces[$i] = hex2bin($pieces[$i]);
                $fkName = Dequalify(foreignKeyname($mainTablename, "$joinTablename.{$joinTableKey[$i]}"));
                $setClause .= "$fkName=".format($pieces[$i],$joinTableKey[$i]).",";
                $i++;
            }
        } else {
             //if it is a foreignKey
            if (in_array("$mainTablename.$name", array_keys($keyMapping))) {   
                $joinTablename = GetTablename($keyMapping["$mainTablename.$name"]);
                
                //if there is also a drop-down box for choosing this foreignKey we take the value of the box
                if (arrayKeyExists("joinTableKeySelector_$joinTablename", $colValues)) 
                    continue;
            }
            $setClause .= "$name=".format($val,$name).",";    
        }
    }
    //we have to delete the last comma
    $setClause = substr($setClause,0,strlen($setClause)-1);
    
    $updateStatement = "UPDATE $mainTablename SET $setClause ";    
    $updateStatement .= "WHERE ";
    
    foreach($tupleKey as $k=>$v)
        $updateStatement .= "$k=".format($v,$k)." AND ";
    
    $updateStatement = substr($updateStatement, 0, strlen($updateStatement)-5);//delete the last and

    $successful = mysql_query($updateStatement) or handle_sql_error(__LINE__, $updateStatement);
    
    //for nm-tables only
    foreach ($nmTablenames as $nmTablename) {
        $connTablename = $connTablenames[$nmTablename];
        $nmTableKey = $nmTables_Key[$nmTablename];
        
        //let's delete all assigned tuples first
        $a = '';
        foreach($tupleKey as $k=>$val) {
            $connTableKeyname = foreignKeyname($connTablename, "$mainTablename.$k");
            $a .= "$connTableKeyname=" . format($val, $k) . " AND ";
        }
        $a = substr($a, 0 , strlen($a)-5);
        
        $delStatement = "DELETE FROM $connTablename WHERE $a";
        $result = mysql_query($delStatement) or handle_sql_error(__LINE__, $delStatement);
        

        if ( isset($HTTP_POST_VARS["assignedTuples"]) ) {
            //now insert all assigned tuples
            $assignedTuples = $HTTP_POST_VARS["assignedTuples"]; //the keys of the assigned tuples
            //assignedTuples will look like this: assignedTuples%5Bmitglied%5D=9;2;1
            $assignedTuples = $assignedTuples[$nmTablename];
            $assignedTuples = explode (";", $assignedTuples);
            
            foreach ($assignedTuples as $assignedTuple) {
                $pieces = explode (",", $assignedTuple);    
                $i=0;
                $iCols = '(';
                $iValues = '(';


                while ($i < count($nmTableKey) ) {
                    $iCols .= Dequalify(foreignKeyname($connTablename,"$nmTablename.$nmTableKey[$i]")) .",";
                    $iValues .= format($pieces[$i], $nmTableKey[$i], $nmTablename) .",";
                    $i++;
                }
                
                foreach($tupleKey as $k=>$v) {
                    $iCols .= Dequalify(foreignKeyname($connTablename, "$mainTablename.$k")).",";
                    $iValues .= format($v,$k) . ",";
                }
                
                $iCols[strlen($iCols)-1] = ')';
                $iValues[strlen($iValues)-1] = ')';
                
                $insertStatement = "INSERT INTO $connTablename $iCols VALUES $iValues";
                
                $successful = mysql_query($insertStatement) or handle_sql_error(__LINE__, $insertStatement);
               
            }
        }
    }
    statusOutput ("Update was successful!");
    die(1);
} else assert(0);


?>

</body>
</html>

<?php
// close the db-connection
mysql_close($dbLink);
?>