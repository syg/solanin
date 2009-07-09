$if(config.setup)$
 <p>There are no restrictions on passwords; be careful!</p>
$else$
 $if(config.ffmpeg)$
  <p>Based on your FFmpeg binary, the following formats are supported:</p>
  <p><b style="color: white;">$config.exts; separator=" "$</b></p>
 $else$
  <p>It looks like you did not point Solanin to a FFmpeg binary.</p>
  <p>You will only be able to stream <b>.mp3</b>s.</p>
 $endif$
 <p>Enter a password that you will be using to log in to complete the configuration.</p>
$endif$

<form id="password-form" action="/_p" method="post">
 <table class="fields">
  <tbody>
   $if(config.setup)$
    <tr>
     <td class="label">Old Password:</td>
     <td class="input">
      $if(validations.oldPassword)$
       <input class="text $if(validations.oldPassword.invalid)$ error $endif$" type="password" name="oldPassword" value="" />
       <div class="error"><span>$validations.oldPassword.invalid$</span></div>
      $else$
       <input class="text" type="password" name="oldPassword" value="" />
      $endif$
     </td>
    </tr>
   $endif$
   <tr>
    <td class="label">New Password:</td>
    <td class="input">
     <input class="text" type="password" name="newPassword" value="" />
    </td>
   </tr>
   <tr>
    <td class="label">Confirm New Password:</td>
    <td class="input">
     $if(validations.confirmPassword)$
      <input class="text $if(validations.confirmPassword.invalid)$ error $endif$" type="password" name="confirmPassword" value="" />
      <div class="error"><span>$validations.confirmPassword.invalid$</span></div>
     $else$
      <input class="text" type="password" name="confirmPassword" value="" />
     $endif$
    </td>
   </tr>
  </tbody>
 </table>
 <div class="buttons">
  $if(!config.setup)$
   <input class="button save" type="submit" value="Finish" />
  $else$
   <input class="button save" type="submit" value="Save" />
   <input class="button cancel" type="submit" value="Cancel" />
  $endif$
 </div>
</form>
