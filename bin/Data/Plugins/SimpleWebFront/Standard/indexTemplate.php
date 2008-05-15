<?php
//| This file is part of the SimpleWebFront-DBDesigner4-Plugin.
// Copyright (C) 2003 Bayer Ulrich, Michael Zinner
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
//|  File:               indexTemplate.php
//|  Author:             Ulrich Bayer, Michael Zinner
//|  CVS:                $Id: indexTemplate.php,v 1.1.1.1 2003/08/20 15:46:22 fabforce_ulli Exp $
//|  Description: 
//|    This is the template for the index.php file
//|    which is the starting point of each webpage.
//|    It shows an overall view of all groups with 
//|    their respective views. 
//|
?>

<?php  $heading = /*[[HEADING]]*/; ?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
            "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
    <meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">
    <meta http-equiv="Content-Script-Type" content="text/javascript">
    <meta http-equiv="Content-Style-Type" content="text/css">
    <meta name="robots" content="noindex">
    <link rel="StyleSheet" href="incs/document.css" type="text/css" media="screen">
    <title><?php echo $heading ?></title>
</head>

<body>

<?php

    //an array of the form (array ("Gruppe1.View1" => "test.png")..
    $imageNames = /*[[IMAGE_NAMES]]*/;

    function println($str)
    {
        print $str;
        print "\n";
    }
    
    function displayGroup ($groupName, $views)
    { 
        global $imageNames;
               
		print '
<!-- group start -->
<table border="0" cellpadding="0" cellspacing="0">
<!-- group header -->
  <tr>
    <td colspan="3">
      <table border="0" cellpadding="0" cellspacing="0">
        <tr>
          <td nowrap><img src="images/1ptrans.png" width="8" height="1"><img src="images/group_header_l.png"></td>
          <td background="images/group_header_m.png"><font class="fontS">'.$groupName.'</font></td>
          <td><img src="images/group_header_r.png"></td>
        </tr>
      </table></td>
  </tr>
<!-- group main -->
  <tr>
    <td width="10"><img src="images/group_lt.png" width="10"></td>
    <td background="images/group_t.png"><img src="images/1ptrans.png" height="9"></td>
    <td width="11"><img src="images/group_rt.png" width="11"></td>
  </tr>
  <tr>
    <td height="100%" background="images/group_l.png"><img src="images/1ptrans.png" width="10"></td>
    <td bgcolor="#f6f6f6">
<!-- group icons -->
      <table border="0" cellpadding="0" cellspacing="0">
        <tr>';
foreach ($views as $viewName => $linkTarget) {
print '          <td align="center"><a href="'.$linkTarget.'"><img src="images/icons/'.$imageNames["$groupName.$viewName"].'" border="0"></a></td>
          <td><img src="images/1ptrans.png" width="10"></td>'; }
print '        </tr>
        <tr>';
foreach ($views as $viewName => $linkTarget) {
print '          <td align="center"><a href="'.$linkTarget.'">'.$viewName.'</td>
          <td>&nbsp;</td>'; }
print '        </tr>
      </table>
    </td>
    <td background="images/group_r.png"><img src="images/1ptrans.png" width="11"></td>
  </tr>
  <tr>
    <td width="10"><img src="images/group_lb.png" width="10"></td>
    <td background="images/group_b.png"><img src="images/1ptrans.png" height="10"></td>
    <td width="11"><img src="images/group_rb.png" width="11"></td>
  </tr>
</table>
<!-- group end -->';        
    }
?>
<br>
<table width="100%" height="37" border="0" cellpadding="0" cellspacing="0" background="images/headerbg.png">
  <td background align="center" class="fontBig"><?php echo $heading ?></td>
</table>
<br>

<table border="0" cellpadding="0" cellspacing="0">
<tr><td><img src="images/1ptrans.png" width="10"></td><td>
//|INDEX is replaced by one(or more) function call(s) to displayGroups
/*[[INDEX]]*/
</td></tr>
</table>

</body>
</html>