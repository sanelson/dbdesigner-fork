function PlacePanelHeader(name, width, align) {
  document.writeln('<table border="0" cellspacing="0" cellpadding="0" align="'+align+'" width="'+width+'" background="images/subtitle_bg.png">');
  document.writeln('  <tr>');
  document.writeln('    <td width="26"><img src="images\/subtitle_leftout.png"><\/td>');
  document.writeln('    <td width="10"><img src="images\/subtitle_left.png"><\/td>');
  document.writeln('    <td background="images\/subtitle_mid.png" width="200">'+name+'<\/td>');
  document.writeln('    <td width="10"><img src="images\/subtitle_right.png"><\/td>');
  document.writeln('    <td>&nbsp;<\/td>');
  document.writeln('    <td width="26"><img src="images\/subtitle_rightout.png"><\/td>');
  document.writeln('  <\/tr>');
  document.writeln('  <tr>');
  document.writeln('    <td colspan="6"><img src="images/1ptrans.png" height="8"><\/td>');
  document.writeln('  <\/tr>');
  document.writeln('<\/table>');
}