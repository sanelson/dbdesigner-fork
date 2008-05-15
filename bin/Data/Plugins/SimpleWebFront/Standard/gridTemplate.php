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
//|  File:               gridTemplate.php
//|  Author:             Ulrich Bayer
//|  CVS:                $Id: gridTemplate.php,v 1.1.1.1 2003/08/20 15:46:28 fabforce_ulli Exp $
//|  Description: 
//|    This is the template-file for all generated grid-
//|    files. The generated grid-file shows all tuples of
//|    the main-Table.
//|     

//include common function definitons
include "commonFunctions.php";

//the include-file connects to the chosen database
/*[[INCLUDE]]*/;
?>

<?php

// Text areas like /*[[EXAMPLE]]*/ will be expanded when processed by the swf
    
    //the name that will appear in the heading
    $viewname = /*[[VIEWNAME]]*/;
    
    //maybe a magic php-var would also have done the job...
    $filename = /*[[FILENAME]]*/;
    
    $mainTablename = /*[[TABLENAME]]*/;
    
    //maintains a column=>Datatype mapping for every column of the mainTable
    //example: /*[[DATATYPES]]*/ could be expanded by something like array("name"=>"INTEGER",...)
    $mainTableDataTypes = /*[[DATATYPES]]*/;
    
    //an array that contains the name of all columns that make up the primary key of the maintable
    //example: /*[[TABLEKEY]]*/ could be expanded by something like array("product_id")
    $mainTableKey = /*[[TABLEKEY]]*/;

    //an array that contains the name of all columns that should be displayed
    //example: /*[[TABLEDISPCOLS]]*/ could be expanded by something like array("product_id", "name","desc")    
    $mainTableDispCols = /*[[TABLEDISPCOLS]]*/;

    //A normal(numeral) array that lists the names of all Join-Tables
    //example: array('testTable1', 'testTable2')
    $joinTablenames = /*[[JOINTABLENAMES]]*/;
    
    //For each joinTable present in $joinTablenames this associative array 
    //keeps an array with the column-names of the key
    //example: array ("product" => array ("productId"), "testTable" => array("Key_part1","Key_part2"))
    $joinTables_Key = /*[[JOINTABLES_KEY]]*/;
    
    //For each joinTable present in $joinTablenames this associative array 
    //keeps an array with the names of all columns that should get displayes
    //example: array( "testTable1" => array( "testTable1Id","comment"), "testTable2" => array( "col1","col2","col3"))
    $joinTables_DispCols = /*[[JOINTABLES_DISPCOLS]]*/;      //array ("Preis");

    //A normal(numeral) array that lists the names of all NM-Tables
    //example: array('testTable1', 'testTable2')
    $nmTablenames = /*[[NMTABLENAMES]]*/;
    
    //For each nmTable present in $joinTablenames this associative array 
    //keeps an array with the column-names of the key
    //example: array ("product" => array ("productId"), "testTable" => array("Key_part1","Key_part2"))
    $nmTables_Key = /*[[NMTABLES_KEY]]*/;
    
    //For each nmTable present in $joinTablenames this associative array 
    //keeps an array with the names of all columns that should get displayes
    //example: array( "testTable1" => array( "testTable1Id","comment"), "testTable2" => array( "col1","col2","col3"))
    $nmTables_DispCols = /*[[NMTABLES_DISPCOLS]]*/;
    
    //For each nmTable present in $joinTablenames this associative array 
    //keeps an (again associative) array that has the form column=>Datatype
    //example: array ("customer" => array ("customerId"=> "INTEGER","Name"=> "VARCHAR"))
    $nmTables_DataTypes = /*[[NMTABLES_DATATYPES]]*/;
    
    //A normal(numeral) array that lists the names of all connection-Tables 
    //(tables that are introduced to realize an n:m relationship)
    //example: array('testTable1', 'testTable2')
    $connTablenames = /*[[CONNECTIONTABLENAMES]]*/;
    
    //(part of) the whereClause
    //doesn't begin with WHERE
    $whereConstraint = /*[[WHERE_CONSTRAINT]]*/;
    
    //the number of data tuples to be shown on a page
    $rowsPerPage= /*[[ROWSPERPAGE]]*/;
    
    //a boolean value that tells us if this (grid-)window is shown as a popup or not
    $gridAsPopup = /*[[GRID_AS_POPUP]]*/;
    
    //the dimensions of the update-window
    $formHeight = /*[[FORMHEIGHT]]*/;
    $formWidth =  /*[[FORMWIDTH]]*/;
    $formX = /*[[FORMX]]*/;
    $formY = /*[[FORMY]]*/;

    //An array describing the (user-defined) captions of all columns that will shown to the user instead of the db-names 
    //The qualified db column-name is used to index the array
    //example: array('product.Name' => 'Name',...)
    $colCaptions =  /*[[COLUMN_CAPTIONS]]*/;
    
    //An associative array that lists all columns that have a width specified with their 
    //respective width-values in pixels
    //The qualified db column-name is used to index the array
    //example: array("testTable1.column1" => 55)
    $colWidths = /*[[COLUMN_WIDTHS]]*/; 
    
    //An associative array that stores all columns that should truncate their contents
    //to a specified number of characters;
    //The qualified db column-name is used to index the array
    //example: array("testTable1.column1" => 5)
    $colTrunc = /*[[COLUMN_TRUNCATE_CHARS]]*/;
   
    //This array stores a mapping of qualified column names to the (qualified) column names they reference
    //Consequently the index consists only of qualified foreign keynames; their value denotes the column
    //they are refering to;
    $keyMapping= /*[[KEY_MAPPING]]*/;
   
    $sortedSelectClause = /*[[SORTED_SELECT_CLAUSE]]*/;

    // array("table.column => type,...);
    //type is one of (main,join,nm);
    $colOrder = /*[[COLUMNS_ORDER]]*/;

    
    $qualified_mainTableKey = Qualify($mainTablename, $mainTableKey);
    
    $tableDispCols = $mainTableDispCols;

    $qualified_tableDispCols = Qualify($mainTablename, $mainTableDispCols);
    foreach ($joinTables_DispCols as $tablename=>$joinTableDispCols)
        $qualified_tableDispCols = array_merge($qualified_tableDispCols, Qualify($tablename, $joinTableDispCols) );
        
    $qualified_nmTableDispCols = array();
    foreach ($nmTables_DispCols as $tablename=>$nmTableDispCols)
        $qualified_nmTableDispCols = array_merge($qualified_nmTableDispCols, Qualify($tablename, $nmTableDispCols) );   

?>

<?php
//function definitions


class MyRow {
    var $firstRow = array(); //a simple array resembling one row of a db-table
    var $nmRows = array();  //a 2-dim array resembling zero or more rows of a db-table
    var $nmLines;
    var $nmTable; //the tuple has at least 1 field on an nmTable
    
    function MyRow($line) {
        global $qualified_nmTableDispCols;
        
        $this->nmLines = 0;
        $this->nmTable = false;   
        
        foreach ($line as $attr => $attrValue) {
                        
            if ((in_array($attr, $qualified_nmTableDispCols)) and ($attrValue != NULL)) { //we won't deal with cols that are not visible
                $this->nmRows[$this->nmLines][$attr] = $attrValue;
                $this->nmTable = true;
            } else {
                $this->firstRow[$attr] = $attrValue;    
            }
        }

        if ($this->nmTable) $this->nmLines++;
    }
    
    //tests if the fieldvalues for all non-nmTable attributes are the same
    function compare($line) {
        if (arrayIntersectAssoc($this->firstRow, $line) == $this->firstRow) return true;
        
        return false;
    }
    
    //should only be called when compare($line) has returned true
    function addNMRow($line) {
        global $qualified_nmTableDispCols;
        
        assert($this->nmTable);
        
        foreach ($line as $attr => $attrValue) {
            if (in_array($attr, $qualified_nmTableDispCols)) {
                $this->nmRows[$this->nmLines][$attr] = $attrValue;
            }
        }
        $this->nmLines++;    
    }
    
    //return type: array()
    function getFirstRow() {
        return $this->firstRow;
    }
    
    function getNMRowsCount() {
        return ($this->nmLines-1);
    }
    
    function getNMRows() {
        return $this->nmRows;
    }
}

function nm_fetch($result) {
    $tuples = array(); //a container for the tuples of the table
    $c=0;
    while ($line = mysql_fetch_array($result, MYSQL_ASSOC)) {
        $added = false;
        $i = 0;
        while ($i < count($tuples) ) {
            
            if (($tuples[$i]->nmTable) && ($tuples[$i]->compare($line))) {
                $tuples[$i]->addNMRow($line);
                $added = true;
                break;
            }
            $i++;
        }
        
        if (!$added) $tuples[$c++] = new MyRow($line);
    }
    return $tuples;
}


//prints the result of the primary sql-query(the one responsible for showing the grid) 
function printResult($result) {
    global $pageNum, $filename, $colCaptions;
    global $qualified_mainTableKey, $qualified_tableDispCols,$qualified_nmTableDispCols;
    global $formWidth, $formHeight, $formX, $formY;
    global $colWidths, $colTrunc;
    global $viewAsGrid;
    global $searchForVars;
    global $HTTP_GET_VARS;
    
    //some vars
    $mode = $HTTP_GET_VARS["mode"]; //$mode is 'normal' or 'search'
    

    //Create Main Data Table
    if (count($colWidths) > 0)    
        print '<table border="0" cellpadding="0" cellspacing="0" style="table-layout:fixed;border-left: 1px solid #C0C0C0;border-bottom:1px solid #606060;border-right:1px solid #606060;" align="center">';
    else
        print '<table border="0" cellpadding="0" cellspacing="0" style="border-left: 1px solid #C0C0C0;border-bottom:1px solid #606060;border-right:1px solid #606060;" align="center">';
    print "\n";
    
    //print the field-names first
    $i=0;
    print "\t<tr>\n";
    while ($i < mysql_num_fields($result)) {
        $fieldInfo = mysql_fetch_field($result, $i);
        $fieldName = $fieldInfo->name; //this is a qualified name in the form of table.column
    
        $order = ''; 
        if ( isset($HTTP_GET_VARS["OrderBy"]) ) {
            $order = $HTTP_GET_VARS["OrderBy"]; 
        }    
        
        if (in_array($fieldName, $qualified_tableDispCols) ) {
            $caption = $colCaptions[$fieldName];
            $colWidth = '';
            if (arrayKeyExists($fieldName,$colWidths))
                $colWidth = "width:{$colWidths[$fieldName]}px;overflow:hidden;";

            //if the table is already sorted after this column then another click should
            //reverse the order
            $orderBy = '';
            if ($order == "a_$fieldName") {
                $fieldName = urlencode($fieldName);
                $orderBy = "&OrderBy=d_{$fieldName}";
            } else {
                $fieldName = urlencode($fieldName);
                $orderBy = "&OrderBy=a_{$fieldName}";
            }                    
            print "\t\t<th class=\"fontS\" style='{$colWidth}background-image:url(images/tbl_headerbg.png);";
            print "padding:3px;border-right:1px solid #C0C0C0'><a href='{$filename}?page=0&mode={$mode}{$orderBy}{$searchForVars}'>";
            print "{$caption}</a></th>\n";        
                
        }
        
        $i++;
    }

    //Add Table Header for Function Column
    print "\t\t<th colspan=\"2\" class=\"fontS\" style=\"background-image:url(images/tbl_headerbg.png); padding:3px;border-right:1px solid #C0C0C0\">&nbsp;</th>\n";
    print "\t</tr>\n";

    //Now handle the data-tuples
    $tuples = nm_fetch($result); 

    foreach ($tuples as $tuple)  {
        $line = $tuple->getFirstRow();
        
        //print a table row
        print "\t<tr>\n";
        
        //print all data values
        foreach($line as $colName => $colValue) {
       
            $colValue = htmlspecialchars($colValue,ENT_QUOTES);
            if (in_array($colName, $qualified_tableDispCols) ) {//only show cols that are meant for display
            
                if (arrayKeyExists($colName, $colTrunc)) {//if we should truncate the data
                    $maxChars = $colTrunc[$colName];
                    if (strlen($colValue) > $maxChars) $colValue = substr($colValue, 0, $maxChars);
                }
                print "\t\t<td style=\"background-color:#FFFFFF;border-bottom:1px solid #C0C0C0; border-right:1px solid #C0C0C0; padding:2px\" class=\"fontN\">$colValue</td>\n";
            }           
        }

        $tupleId = '';
        foreach ($qualified_mainTableKey as $k) {
            $tupleId .= "&keyValues%5B".urlencode(Dequalify($k))."%5D=".urlencode($line[$k]);
        }
        $sql_filename = substr($filename,0,strlen($filename)-4) . '_sql.php';        
        $url = "$sql_filename?mode=del". $tupleId;
        
        //print the delete symbol for each row
        print "\t\t<td style=\"background-color:#FFFFFF;border-bottom:1px solid #C0C0C0;vertical-align:middle;padding-left:2px;\"><a href=\"javascript: void deleteRow('$url')\">";
        print '<img src="images/tool_smdel.png" width="15" height="16" alt="Delete-Button" style="cursor:pointer">';
        print '</a></td>';
              
        //print the update symbol for each row
        $updateFilename = substr($filename,0,strlen($filename)-4) . '_update.php';
        print "\t\t<td style=\"background-color:#FFFFFF;border-bottom:1px solid #C0C0C0; border-right:1px solid #C0C0C0;vertical-align:middle;padding-right:2px\"><a href='javascript: void window.open(\"{$updateFilename}?mode=showForm{$tupleId}\",\"UpdateWindow\",\"width={$formWidth},height=$formHeight,left=$formX,top=$formY,scrollbars=yes,status=no,toolbar=no,resizable=no,dependent=yes\")'>";
        print '<img src="images/tool_smedit.png" width="34" height="16" alt="Edit-Button" style="cursor:pointer"></a></td>';
        
        //print the end of the row
        print "\t</tr>\n";
        
        //now print the nm-table-entries
        $lines = $tuple->getNMRows();

        //If there are n:m tuppels
        if(count($lines)>0){
            print "<tr>\n";
            print "<td colspan='$i' style=\"background-color:#F0F0F0\">";
            print "<table border='0' cellspacing='0' cellpadding='0'>";
            $firstLine = true;
            foreach ($lines as $line) {
                // Write n:m Tuppel Header
                if ($firstLine) {
                    // Write Left Spacer
                    print "<tr><td style=\"background-color:#F0F0F0;border-right:1px solid #D0D0D0;\" class=\"fontSM\"><img src=\"images/1ptrans.png\" width=\"25\" height=\"1\"></td>";
  
                    // Write each Header
                    foreach ($line as $colName => $colValue) {                        
                        print "<td class=\"fontSM\" style=\"background-color:#F8F8F8;border-bottom:1px solid #D0D0D0;border-right:1px solid #D0D0D0;padding:1px;padding-left:2px\">$colCaptions[$colName]</td>";
                    }
                    print "</tr>";
                    $firstLine=false;
                }
                // Write Data

                // Write Left Spacer
                print "<tr><td style=\"background-color:#F0F0F0;border-right:1px solid #D0D0D0;\" class=\"fontSM\"><img src=\"images/1ptrans.png\" width=\"13\" height=\"1\"><img src=\"images/tbl_nm_record.png\"></td>";
                // Write each Data cell
                foreach ($line as $colName => $colValue) {
                    $colValue = htmlspecialchars($colValue,ENT_QUOTES);
                    print "<td class=\"fontNM\" style=\"background-color:#F8F8F8;border-bottom:1px solid #D0D0D0;border-right:1px solid #D0D0D0;padding-left:2px;padding-right:4px;padding-top:1px;padding-bottom:1px\">$colValue</td>";
                }
                print "</tr>";
            }
            print "</table>";
            print "</td>\n";
            print "</tr>\n";
        }
        print "<tr><td colspan='$i' style=\"background-color:#F0F0F0;border-bottom:1px solid #D0D0D0;\"><img src=\"images/1ptrans.png\" height=\"5\"></td></tr>\n";
    }
    print "</table>\n";
}


//finds out how many tuples we have and then calculates the number of the last page
function getLastPage($query,$rowsPerPage) {

    $pos = strpos ($query, "FROM");
    $partQuery = substr($query, $pos); //string substr ( string string, int start [, int length])
    $countQuery = "SELECT COUNT(*) " . $partQuery;
            
    $result = mysql_query($countQuery) or die("Error: Sql-query $countQuery was not successful!");
    $row = mysql_fetch_array($result, MYSQL_NUM);
    $tupleCount = $row[0];
       
    //free the query-result
    mysql_free_result($result);
    
    return (int) ($tupleCount / $rowsPerPage);
}

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
  <title><?php echo $viewname ?></title>

  <script type="text/javascript">
    function deleteRow(url) {
      if (confirm('Do your really want to delete this record?')) { 
          top.Status_section.location.href = url;
      }
    }
  </script>

</head>

<body>

<img src="images/1ptrans.png" height="10"><br>
<table border="0" cellpadding="0" cellspacing="0" align="center">
    <tr> <!-- table heading -->
        <td><script type="text/javascript"> <!-- 
        PlacePanelHeader("<?php echo $viewname ?>", '400', 'left');  //-->
        </script>
        </td>
    </tr>
    <tr> <!-- data -->
        <td><?php

            //get the current page
            $pageNum= $HTTP_GET_VARS["page"];
            
            $numPages = $pageNum + 1; //since we start counting with 0
            $beginOffset = ($numPages-1) * $rowsPerPage;
            
            //construct the select-clause
            /*$mainTableCols = array_unique(array_merge($mainTableKey, $mainTableDispCols));
            $selectClause = QualifyAndAlias($mainTablename,$mainTableCols);
            
            foreach ($joinTablenames as $joinTablename)
                $selectClause = array_merge( $selectClause, QualifyAndAlias($joinTablename,$joinTables_DispCols[$joinTablename]) );    
            
            foreach ($nmTablenames as $nmTablename)
                $selectClause = array_merge( $selectClause, QualifyAndAlias($nmTablename, $nmTables_DispCols[$nmTablename]) );
            */
            //$selectClause = implode(',', $selectClause);
            $selectClause = $sortedSelectClause;
            
            //construct the From and Where-Clause
            $fromClause = $mainTablename;
            $whereClause = '';
            
            if (count($nmTablenames) == 0) {
                //From-clause
                foreach ($joinTablenames as $joinTablename)
                    $fromClause .= ",$joinTablename";
                foreach ($nmTablenames as $nmTablename)
                    $fromClause .= ",$nmTablename";
                foreach ($connTablenames as $connTablename)
                    $fromClause .= ",$connTablename";
                
                //Where-clause
                if ( (count($joinTablenames) >0 ) || (count($nmTablenames) >0) || (strlen($whereConstraint) >0) ) {
                   $whereClause = 'WHERE '; 
                }
                foreach ($joinTablenames as $joinTablename) {
                    $joinTableKey = $joinTables_Key[$joinTablename];
                    foreach($joinTableKey as $keyElement) {
                        $qualifiedKeyElement = "$joinTablename.$keyElement";
                        $fKeyname = foreignKeyname($mainTablename,$qualifiedKeyElement);
                        $whereClause .= "$fKeyname = $qualifiedKeyElement AND ";   
                    }
                }
                                      
            } else { //we have at least one nmTable that wants to get shown
            
                if ( (count($joinTablenames) >0 ) || (strlen($whereConstraint) >0) ) {
                   $whereClause = 'WHERE '; 
                }   
                foreach ($nmTablenames as $nmTablename) {
                    $connTablename = $connTablenames[$nmTablename];
                    $nmTableKey = $nmTables_Key[$nmTablename];
                    
                    $fromClause .= " LEFT OUTER JOIN $connTablename ON ";
                    foreach ($mainTableKey as $keyElement) {
                        $qualifiedKeyElement = "$mainTablename.$keyElement";
                        $connTableKeyname = foreignKeyname($connTablename,$qualifiedKeyElement);
                        $fromClause .= "$qualifiedKeyElement = $connTableKeyname AND ";
                    }
                    $fromClause = substr($fromClause,0, strlen($fromClause)-5);
                    
                    $fromClause .= " LEFT OUTER JOIN $nmTablename ON ";
                    foreach ($nmTableKey as $keyElement) {
                        $qualifiedKeyElement = "$nmTablename.$keyElement";
                        $connTableKeyname = foreignKeyname($connTablename,$qualifiedKeyElement);
                        $fromClause .= "$qualifiedKeyElement = $connTableKeyname AND ";
                    }
                    $fromClause = substr($fromClause,0, strlen($fromClause)-5);
                }
                foreach ($joinTablenames as $joinTablename)
                    $fromClause .= ",$joinTablename";

                //construct the where-clause
                foreach ($joinTablenames as $joinTablename) {
                    $joinTableKey = $joinTables_Key[$joinTablename];
                    foreach($joinTableKey as $keyElement) {
                        $qualifiedKeyElement = "$joinTablename.$keyElement";
                        $fKeyname = foreignKeyname($mainTablename,$qualifiedKeyElement);
                        $whereClause .= "$fKeyname = $qualifiedKeyElement AND ";   
                    }
                }  
            }
                        
            if (strlen($whereConstraint)>0)             
                $whereClause .= $whereConstraint;
            else //$whereClause could also be the empty string but even then substr doesn't do any harm
                $whereClause = substr($whereClause,0, strlen($whereClause)-5);   //delete the last ' AND '

            //this variable will save the searchVariables so that we can pass them on
            $searchForVars='';
            if ($HTTP_GET_VARS["mode"] == "search") {
                
                $counter = -1;                
                foreach ($colOrder as $qualField=>$type) {
                    
                    if (($type == "main") or ($type == "join")) { 
                        //we can use the counter, since $colOrder is the same in searchTemplate.php
                        $counter++;
                        
                        if (! isset($HTTP_GET_VARS["a_$counter"]) ) continue;
                        $value = $HTTP_GET_VARS["a_$counter"];
                        
                        if ($value == "") continue;

                        $url_value = urlencode($value);
                        $searchForVars .= "&a_$counter=$url_value";

                        if ($whereClause == '')
                            $whereClause .= "WHERE $qualField LIKE '$value%'";                                                
                        else
                            $whereClause .= " AND $qualField LIKE '$value%'";                        
                    }
                }                             
            }

           
            //finally we have our select-query
            $query = "SELECT $selectClause FROM $fromClause $whereClause";     
      
            if ( isset($HTTP_GET_VARS["OrderBy"]) and ($HTTP_GET_VARS["OrderBy"] != '') ) {
                $order = $HTTP_GET_VARS["OrderBy"]; //order is a_qualifiedFieldname or d_qualifiedFieldname
                $col_only = substr($order,2);
                $query .= " ORDER BY $col_only";
                
                $direction = substr($order,0,2);
                if ($direction == "d_") $query .= " DESC";
                else if ($direction == "a_") $query .= " ASC";
                else assert(0);
                
            }
                       
            //limit-Query
            $lQuery = $query . " LIMIT " . $beginOffset . "," . $rowsPerPage;

    
            $result = mysql_query($lQuery) or handle_sql_error(__LINE__, $lQuery);

            $lastPage=false;
            if (mysql_num_rows($result) < $rowsPerPage) {
                $lastPage=true;
            }

            //print the result in an HTML-table
            if (mysql_num_rows($result) == 0) {
                print "Found no data records.<br>";
            } else {
                printResult($result);
            }

            //Free the result
            mysql_free_result($result);
           
            ?>
        </td>
    </tr>
    <tr> <!-- toolbar -->
        <td>
            <table border="0" cellspacing="0" cellpadding="0" align="center">
                <tr>
                    <td><img src="images/1ptrans.png" height="10" width="1" alt=""></td>
                </tr>
                <tr>
                    <td height="33" align="center" valign="top" nowrap>
                        <?php 
                         ?>
<?php
$frame_filename = substr($filename,0,strlen($filename)-4) . '_frame.php';
$orderBy="";
if ( isset($HTTP_GET_VARS["OrderBy"]) ) {
    $orderCol = $HTTP_GET_VARS["OrderBy"];
    $orderCol = urlencode($orderCol);
    $orderBy="&OrderBy=$orderCol";
}
$mode = $HTTP_GET_VARS["mode"];
//The first-page button
if($pageNum>0) {
    echo "<a href='{$filename}?page=0&mode={$mode}{$orderBy}{$searchForVars}'>";
    echo "<img src='images/btnfirst.png' width='43' height='33' alt='FirstPage-Button'>";
    echo "</a>\n";
} else {
    echo "<img src='images/btnfirst_disabled.png' width='43' height='33' alt='FirstPage-Button_disabled'>\n";
}
//The prev-page button
if($pageNum>0) {
    echo "<a href='{$filename}?page=".($pageNum-1)."&mode={$mode}{$orderBy}{$searchForVars}'>";
    echo "<img src='images/btnprev.png' width='43' height='33' alt='PrevPage-Button'>";
    echo "</a>\n";
} else {
    echo "<img src='images/btnprev_disabled.png' width='43' height='33' alt='PrevPage-Button_disabled'>\n";
}
//The next-page button
if(!$lastPage) {
    echo "<a href='{$filename}?page=".($pageNum+1)."&mode={$mode}{$orderBy}{$searchForVars}'>";
    echo "<img src='images/btnnext.png' width='43' height='33' alt='NextPage-Button'>";
    echo "</a>\n";
} else {
    echo "<img src='images/btnnext_disabled.png' width='43' height='33' alt='NextPage-Button_disabled'>\n";
}
//The last-page button
if(!$lastPage) {
    echo "<a href='{$filename}?page=".(getLastPage($query,$rowsPerPage))."&mode={$mode}{$orderBy}{$searchForVars}'>";
    echo "<img src='images/btnend.png' width='43' height='33' alt='LastPage-Button'>";
    echo "</a>\n";
} else {
    echo "<img src='images/btnend_disabled.png' width='43' height='33' alt='LastPage-Button_disabled'>\n";
}
//The home-button
if($gridAsPopup==0) 
    echo "<a href='index.php' target='_top'><img src='images/btnhome.png'></a>\n";
else
    echo "<a href='javascript: window.close();'><img src='images/btnhome.png'></a>\n";
//The New-Record button
$insertFilename = substr($filename,0,strlen($filename)-4) . '_insert.php'; 
$jscript_window_open = "javascript: void window.open(\"{$insertFilename}?\",\"InsertWindow\",\"width={$formWidth},height=$formHeight,left=$formX,top=$formY,scrollbars=yes,status=no,toolbar=no,resizable=no,dependent=yes\")";
echo "<a href='$jscript_window_open'>";
echo "<img src='images/btnnew.png'>";
echo "</a>\n";
//The search button
$searchFilename = substr($filename,0,strlen($filename)-4) . '_search.php'; 

if ($mode == "normal")
    print "<a href='$frame_filename?page={$pageNum}&mode=search$orderBy'><img src='images/btnsearch.png'></a>\n";
else if ($mode == "search")
    print "<a href='$filename?page=0&mode=normal$orderBy' target='_parent'><img src='images/btnsearch.png'></a>\n";
else assert(0);
?>
 
                    </td>          
                </tr>
            </table>
        </td>
    </tr>
</table>


</body>
</html>

<?php
// close the db-connection
mysql_close($dbLink);
?>
