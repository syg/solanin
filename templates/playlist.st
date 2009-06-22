$if(!first(dirs))$
 $if(!first(songs))$
  <div class="nofiles">no playable files found</div>
 $endif$
$endif$

<ul class="files">
 $dirs:{ d |
  <li class="dir"><a href="$d.directory$">$d.basename$</a></li>
 }$
 $if(songs)$
 <table class="songs">
  <tbody>
  $songs:{ s |
    <tr>
     <td class="no">
      <div>$s.track$</div>
     </td>
     <td class="title">
      <div class="hl-left"></div>
      <div class="hl-right"></div>
      <a href="$s.song$">$s.title$</a>
     </td>
     <td class="artist"><span>$s.artist$</span></td>
     <td class="duration"></div>$s.duration$</div></td>
    </tr>
  }$
  </tbody>
 </table>
 $endif$
</ul>
