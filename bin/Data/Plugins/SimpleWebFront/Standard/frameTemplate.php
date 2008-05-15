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
//|  File:               frameTemplate.php
//|  Author:             Ulrich Bayer
//|  CVS:                $Id: frameTemplate.php,v 1.1.1.1 2003/08/20 15:46:25 fabforce_ulli Exp $
//|  Description: 
//|    For each view a frame.php page is generated that
//|    is based on this template. The search page shows
//|    a form where the user can enter his search query
//|    and transmits the query to the grid-page via java-
//|    script.
//|
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"
            "http://www.w3.org/TR/html4/frameset.dtd">
<html>
<head>
</head>

<?php
    $filename = /*[[FILENAME_GRID]]*/;


    $mode = $HTTP_GET_VARS["mode"];
    if ($mode == "normal") $statusHeight="20";
    else if ($mode == "search") $statusHeight="100";
    else assert(0);
    
    echo "<frameset rows='*,$statusHeight' frameborder='0' framespacing='0' bordercolor='#F0F0F0' border='0'>\n";

    //get the current page
    $pageNum= $HTTP_GET_VARS["page"];
    
    
    $orderBy="";
    if ( isset($HTTP_GET_VARS["OrderBy"]) ) {
        $orderCol = $HTTP_GET_VARS["OrderBy"];
        $orderBy="&OrderBy=$orderCol";
    }
    
    
    if ($mode == "search") {
        echo "<frame src='$filename?page=$pageNum&mode=search{$orderBy}' name='Main_Main' scrolling='auto'>\n";
        $strippedFilename = substr($filename,0,strlen($filename)-4);
        echo "<frame src='{$strippedFilename}_search.php' name='Search_section' scrolling='auto'>\n";
    } else {
        echo "<frame src='$filename?page=$pageNum&mode=normal{$orderBy}' name='Main' scrolling='auto'>\n";
        echo "<frame src='pure.html' name='Status_section' scrolling='auto'>\n";
    }
?>

  <noframes>
    Your Browser doesn't support framesets.
  </noframes>
</frameset>

</html>