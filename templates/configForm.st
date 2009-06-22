$if(config.setup)$
 $if(config.ffmpeg)$
  $if(config.exts)$
   <p>Your current FFmpeg binary can transcode the following formats:</p>
   <p><b>$config.exts; separator =" "$</b></p>
  $endif$
 $else$
  <p>Solanin is currently not configured to use FFmpeg.</p>
 $endif$
 <p>If you change the location of the FFmpeg binary, open this page again after saving to see what formats are supported.</p>
$endif$

<form id="config-form" action="/_c" method="post">
 <table class="fields">
  <tbody>
   <tr>
    <td class="label">Library Location:</td>
    <td class="input">
     $if(validations.library)$
      <input class="text $if(validations.library.invalid)$ error $endif$" type="text" name="library" value="$validations.library.valid$" />
      <div class="error"><span>$validations.library.invalid$</span></div>
     $else$
      <input class="text" type="text" name="library" value="$config.library$" />
     $endif$
    </td>
   </tr>
   <tr>
    <td class="label">
     Ignored Directories:
     <div class="label-help"><span>(comma-separated)</span></div>
    </td>
    <td class="input">
     $if(validations.ignored)$
      <input class="text $if(validations.ignored.invalid)$ error $endif$" type="text" name="ignored" value="$validations.ignored.valid$" />
      <div class="error"><span>$validations.ignored.invalid$</span></div>
     $else$
      <input class="text" type="text" name="ignored" value="$config.ignored$" />
     $endif$
    </td>
   </tr>
   <tr>
    <td class="label">FFmpeg Binary:</td>
    <td class="input">
     $if(validations.ffmpeg)$
      <input class="text $if(validations.ffmpeg.invalid)$ error $endif$" type="text" name="ffmpeg" value="$validations.ffmpeg.valid$" />
      <div class="error"><span>$validations.ffmpeg.invalid$</span></div>
     $else$
      <input class="text" type="text" name="ffmpeg" value="$config.ffmpeg$" />
     $endif$
    </td>
   </tr>
   <tr>
    <td class="label">Transcode Bitrate:</td>
    <td class="input">
     $if(validations.bitrate)$
      <input class="text $if(validations.bitrate.invalid)$ error $endif$" type="text" name="bitrate" value="$validations.bitrate.valid$" />
      <div class="error"><span>$validations.bitrate.invalid$</span></div>
     $else$
      <input class="text" type="text" name="bitrate" value="$config.bitrate$" />
     $endif$
    </td>
   </tr>
  </tbody>
 </table>
 <div class="buttons">
  $if(!config.setup)$
   <input class="button save" type="submit" value="Next" />
  $else$
   <input class="button save" type="submit" value="Save & Reload" />
   <input class="button cancel" type="submit" value="Cancel" />
  $endif$
 </div>
</form>
