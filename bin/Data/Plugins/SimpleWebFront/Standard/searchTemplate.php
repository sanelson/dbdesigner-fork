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
//|  File:               searchTemplate.php
//|  Author:             Ulrich Bayer
//|  CVS:                $Id: searchTemplate.php,v 1.1.1.1 2003/08/20 15:46:22 fabforce_ulli Exp $
//|  Description: 
//|    For each view a search.php page is generated that
//|    is based on this template. The search page shows
//|    a form where the user can enter his search query
//|    and transmits the query to the grid-page via java-
//|    script.
//|
?>
<?php
    //the name that will appear in the heading
    $viewname = /*[[VIEWNAME]]*/;    
    $frame_filename = /*[[FILENAME_FRAME]]*/;
    $grid_filename = /*[[FILENAME_GRID]]*/;
    
    
    // array("table.column => type,...);
    //type is one of (main,join,nm);
    $colOrder = /*[[COLUMNS_ORDER]]*/;


    //An array describing the (user-defined) captions of all columns that will shown to the user instead of the db-names 
    //The qualified db column-name is used to index the array
    //example: array('product.Name' => 'Name',...)
    $colCaptions =  /*[[COLUMN_CAPTIONS]]*/;
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
            "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">
  <link rel="StyleSheet" href="incs/document.css" type="text/css" media="screen">
  <script type="text/javascript" src="incs/dlg.js"></script> 
  <script type="text/javascript">
    launchingSearch = 0;
    function submit_queue() {
        launchingSearch++;
        window.setTimeout("mySubmit()", 600);    
    }
    function mySubmit() {
        //only search for the last key down
        launchingSearch--;
        if(launchingSearch > 0) return;
                
        document.forms[0].submit(); 
    }
    function exitSearch() {
        var arguments = parent.Main_Main.location.search;
        var gridFilename = <?php echo "'$grid_filename'" ?>;
        
        //find out the value of OrderBy
        var regex = /.+&OrderBy=(.+)&*/ ;
        regex.exec(arguments);
        var orderBy = RegExp.$1;
        if (orderBy != '') orderBy = '&OrderBy=' + orderBy;
               
        parent.location.href = gridFilename + '?page=0&mode=normal'+ orderBy;        
    }
    function fillInOrderBy() {
        //get the string after ?
        var arguments = parent.Main_Main.location.search;
        
        //find out the value of orderBy
        var regex = /.+&OrderBy=(.+)&*/ ;
        regex.exec(arguments);
        var orderByValue = RegExp.$1;
        
        //set the value
        document.SearchForm.OrderBy.value = orderByValue;

	//true means that we allow the form to be submitted
        return true;
   }
  </script> 
</head>
<body background="images/winbg.png">
<?php echo "<form name='SearchForm' Method='GET' action='$grid_filename' target='Main_Main' OnSubmit='fillInOrderBy()'>"; ?>

<img src="images/1ptrans.png" height="4"><br>

<table border="0" cellpadding="0" cellspacing="0" align="center">
  <tr>
    <td><script type="text/javascript"> <!-- 
PlacePanelHeader("<?php echo 'Search '.$viewname ?>", '400', 'left'); //--></script></td>
  </tr>
  <tr>
    <td>
      <table border="0" cellpadding="1" cellspacing="0" style="border: 1px solid #000000">
        <tr> <!-- first write the headings -->
<?php     
    foreach ($colOrder as $qualfield => $type) {
        if (($type == 'main') or ($type == 'join')) {

            $caption = $colCaptions[$qualfield];
            echo "<td class='fontS'>&nbsp;{$caption}</td>"; 
        }
    }
    echo "<td valign='top' class='fontS' rowspan='2'>";
    echo "<a href=\"javascript: void (exitSearch())\"><img src='images/red_close.png' border='0' align='right'></a></td>";
    
?>
        </tr>
        <tr> <!-- now the input-fields -->
        
<?php
    $counter = 0; 
    foreach ($colOrder as $qualfield => $type) {
        if (($type == 'main') or ($type == 'join')) {
            echo "<td valign='top'> <input style=\"width:80px;\" type='text' name='a_{$counter}' OnKeyUp=\"submit_queue()\">  </td>"; 
            $counter++;
        }
    }
   
?>

        
        </tr>
      </table>
    </td>
  </tr>
</table>
<!-- some hidden fields for transporting data -->
<input type='hidden' name='page' value='0'>
<input type='hidden' name='mode' value='search'>
<input type='hidden' name='OrderBy' value=''>
</form>
</body>

</html>