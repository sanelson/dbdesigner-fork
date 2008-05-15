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

/*
 * File:               commonFunctions.php
 * Author:             Ulrich Bayer
 * CVS:                $Id: commonFunctions.php,v 1.1.1.1 2003/08/20 15:46:25 fabforce_ulli Exp $
 * Description: 
 *   contains common function definitions
 */
   

//function definitions

//returns the foreignKeyname of a key in a given table
function foreignKeyname($tablename, $rKeyname) {
    global $keyMapping;

    foreach ($keyMapping as $fk => $keyname)
        if (($rKeyname == $keyname) && (GetTablename($fk) == $tablename)) return $fk;

    assert(false);
}
    
//returns an array with qualified fieldNames
//example: Qualify ("someTable", array ("name", "sNr")  )
//         returns ("someTable.name", "someTable.snr")
function Qualify($tablename, $colArray) {
    $i =0;
    $result = array ();
        
    foreach($colArray as $el)
        $result[$i++] = "$tablename.$el";
            
    return $result;
}

//Qualifies and makes an alias of each element in the array
function QualifyAndAlias($tablename, $colArray) {
    $i =0;
    $result = array ();
        
    foreach($colArray as $el)
        $result[$i++] = "$tablename.$el AS '$tablename.$el'";
            
    return $result;
}

//Returns "field" if called with "table.field"
function Dequalify($colName) {
    $result = '';
    
    $pos = strrpos($colName, ".");
    $result = substr($colName,$pos+1);
 
    return $result;       
}

//Returns the tablename of a qualified fieldname
function GetTablename($colName) {
    $result = '';
    
    $pos = strrpos($colName, ".");
    $result = substr($colName,0,$pos);
 
    return $result;       
}

//encloses the $fieldValue in '' pairs if necessary (= if the value is a string)
function format($fieldValue,$fieldname, $tablename='default') {
    global $mainTableDataTypes, $nmTables_DataTypes, $mainTablename;
    
    $result = '';
    if ($tablename == 'default') $tablename = $mainTablename;
    
    if ( $tablename == $mainTablename ) { //mainTable
        //if ( (substr_count($mainTableDataTypes[$fieldname],'VARCHAR') > 0) || ($mainTableDataTypes[$fieldname] == 'DATE') )
            $result = "'$fieldValue'";
        //else
        //    $result = "$fieldValue";
    }elseif ( isset($nmTables_DataTypes[$tablename]) ) { //nmTable
        //if ( (substr_count($nmTables_DataTypes[$tablename][$fieldname],'VARCHAR') > 0)  || ($mainTableDataTypes[$fieldname] == 'DATE') )
            $result = "'$fieldValue'";
        //else
        //    $result = "$fieldValue";   
    }else
        assert(0);
    
    return $result;        
}

//For PHP versions before 4.1.0 (replacement for array_key_exists())
function arrayKeyExists($key, $search) {
   if (in_array($key, array_keys($search))) {
       return true;
   } else {
       return false;
   }
}

//Tests if two arrays are equal;
//Equal means that they have the same key=>value pairs
function arraysEqual($array1, $array2) {
    
    //they must have the same length
    $c = count($array1);
    $d = count($array2);
    if ($c != $d) return false;
    
    foreach($array1 as $key=>$val) {
        if (! arrayKeyExists($key, $array2)) return false;
        if ($array2[$key] != $val) return false;
    }
    
    return true;        
} 

//For PHP versions before 4.3.0 (replacement for array_intersect_assoc)
//Computes the intersection of arrays with additional index check
function arrayIntersectAssoc($array1, $array2) {
    $returnArray = array();
    
    foreach($array1 as $key=>$val) {
        if ( (arrayKeyExists($key, $array2)) and ((string) $array2[$key] === (string) $val) ) {
            $returnArray[$key] = $val;
        }
    }    
    
    return $returnArray;
}

function hex2bin($hexdata) {
  
  $bindata = ''; 
  for ($i=0;$i<strlen($hexdata);$i+=2) {
    $bindata .= chr(hexdec(substr($hexdata,$i,2)));
  }
 
  return $bindata;
}

?>