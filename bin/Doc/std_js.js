function newImage(arg) {
	if (document.images) {
		rslt = new Image();
		rslt.src = arg;
		return rslt;
	}
}

function ImageHeight(arg) {
	theImg = newImage(arg);
	return theImg.height;
}


function PlaceImgWithShadow(imgname, subtitle) {
document.writeln('<table border=0 cellpadding=0 cellspacing=0 align="center">');
document.writeln('  <tr>');
document.writeln('    <td style="border-top: 1px solid #f0f0f0; border-left: 1px solid #f0f0f0; border-bottom: 2px solid #e0e0e0; border-right: 2px solid #e0e0e0">');
document.writeln('      <table border=0 cellpadding=0 cellspacing=0 align="center">');
document.writeln('        <tr>');
document.writeln('          <td style="border-top: 1px solid #d8d8d8; border-left: 1px solid #d8d8d8; border-bottom: 1px solid #b0b0b0; border-right: 1px solid #b0b0b0"><img src="'+imgname+'"><\/td>');
document.writeln('        <\/tr>');
document.writeln('      <\/table>');
document.writeln('    <\/td>');
document.writeln('  <\/tr>');
document.writeln('  <tr>');
document.writeln('    <td class="Footnote" align="right">'+subtitle);
document.writeln('    <\/td>');
document.writeln('  <\/tr>');
document.writeln('<\/table>');
if(1==2) {
document.writeln('<table border=0 cellpadding=0 cellspacing=0 align="center">');
document.writeln('  <tr>');
document.writeln('    <td colspan="3">');
document.writeln('      <table border=0 cellpadding=0 cellspacing=0 background="images\/s\/s_top.gif" width="100%">');
document.writeln('        <tr>');
document.writeln('          <td>');
document.writeln('            <table border=0 cellpadding=0 cellspacing=0 background="" width="100%">');
document.writeln('              <tr>');
document.writeln('                <td width="10px"><img src="images\/s\/s_top_left_corner.gif"><img src="images\/s\/s_top_left_corner_h.gif"><\/td>');
document.writeln('                <td><img src="images\/1ptrans.gif"><\/td>');
document.writeln('                <td width="10px"><img src="images\/s\/s_top_right_corner_h.gif"><img src="images\/s\/s_top_right_corner.gif"><\/td>');
document.writeln('              <\/tr>');
document.writeln('            <\/table>');
document.writeln('          <\/td>');
document.writeln('        <\/tr>');
document.writeln('      <\/table>');
document.writeln('    <\/td>');
document.writeln('  <\/tr>');
document.writeln('  <tr>');
document.writeln('    <td>');
document.writeln('      <table border=0 cellpadding=0 cellspacing=0 height="'+ImageHeight(imgname)+'px" style="padding: 0px">');
document.writeln('        <tr>');
document.writeln('          <td height="4px"><img src="images\/s\/s_top_left_corner_v.gif"><\/td>');
document.writeln('        <\/tr>');
document.writeln('        <tr>');
document.writeln('          <td><img src="images\/s\/s_left.gif" width="4" height="'+(ImageHeight(imgname)-12)+'"><\/td>');
document.writeln('        <\/tr>');
document.writeln('        <tr>');
document.writeln('          <td height="4px"><img src="images\/s\/s_bottom_left_corner_v.gif"><\/td>');
document.writeln('        <\/tr>');
document.writeln('      <\/table>');
document.writeln('    <\/td>');
document.writeln('    <td><img src="'+imgname+'"><\/td>');
document.writeln('    <td>');
document.writeln('      <table border=0 cellpadding=0 cellspacing=0 height="'+ImageHeight(imgname)+'px" style="padding: 0px">');
document.writeln('        <tr>');
document.writeln('          <td height="4px"><img src="images\/s\/s_top_right_corner_v.gif"><\/td>');
document.writeln('        <\/tr>');
document.writeln('        <tr>');
document.writeln('          <td><img src="images\/s\/s_right.gif" width="4" height="'+(ImageHeight(imgname)-12)+'"><\/td>');
document.writeln('        <\/tr>');
document.writeln('        <tr>');
document.writeln('          <td height="4px"><img src="images\/s\/s_bottom_right_corner_v.gif"><\/td>');
document.writeln('        <\/tr>');
document.writeln('      <\/table>');
document.writeln('    <\/td>');
document.writeln('  <\/tr>');
document.writeln('  <tr>');
document.writeln('    <td colspan="3">');
document.writeln('      <table border=0 cellpadding=0 cellspacing=0 background="images\/s\/s_bottom.gif" width="100%" style="padding: 0px">');
document.writeln('        <tr>');
document.writeln('          <td>');
document.writeln('            <table border=0 cellpadding=0 cellspacing=0 background="" width="100%">');
document.writeln('              <tr>');
document.writeln('                <td width="10px"><img src="images\/s\/s_bottom_left_corner.gif"><img src="images\/s\/s_bottom_left_corner_h.gif"><\/td>');
document.writeln('                <td><img src="images\/1ptrans.gif"><\/td>');
document.writeln('                <td width="10px"><img src="images\/s\/s_bottom_right_corner_h.gif"><img src="images\/s\/s_bottom_right_corner.gif"><\/td>');
document.writeln('              <\/tr>');
document.writeln('            <\/table>');
document.writeln('          <\/td>');
document.writeln('        <\/tr>');
document.writeln('      <\/table>');
document.writeln('    <\/td>');
document.writeln('  <\/tr>');
document.writeln('  <tr>');
document.writeln('    <td colspan="3" class="Footnote" align="right">');
document.writeln('      '+subtitle);
document.writeln('    <\/td>');
document.writeln('  <\/tr>');
document.writeln('<\/table>');}}

function PlaceImg(imgname, subtitle) {
document.writeln('<table border=0 cellpadding=0 cellspacing=0 align="center">');
document.writeln('  <tr>');
document.writeln('    <td class="Footnote" align="right"><img src="'+imgname+'"><br>'+subtitle);
document.writeln('    <\/td>');
document.writeln('  <\/tr>');
document.writeln('<\/table>');}
