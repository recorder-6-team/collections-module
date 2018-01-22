{ -----------------------------------------------------------------------------
 Unit:        ErrorMessages

              Copyright © Dorset Software Services Ltd, 2000

 Description: Messages used by exceptions and/or message boxes

 Notation used in prefixing the constants names (just a thought really):
    EST_ : Error strings, to use with message dialogs.

 Author:      Eric Salmon

 Created:     16/03/2000

 Changes:
 ----------------------------------------------------------------------------- }

unit ErrorMessages;

interface

const
  // Login
  EST_LOCATE_USER_FAIL = 'Unable to find the user in the database.  '#13+
                         'The user may have been deleted.';
  EST_NO_USERS         = 'There are no users in the database.  The application cannot start.';
  EST_WRONG_PASSWORD   = 'The User Name and Pasword are incorrect.  Please ensure you '#13+
                         'have entered a valid User Name and Password.';

  // Change Password
  EST_NEW_MISSING          = 'Please enter a new password';
  EST_CONFIRM_MISSING      = 'Please confirm your password';
  EST_NEW_PASS_FAIL        = 'Your new password must be different from your old password';
  EST_CONFIRM_FAIL         = 'New password and confirmation do not match';
  EST_CHANGE_PASSWORD_FAIL = 'Unable to change the password';
  EST_CP_WRONG_PASSWORD    = 'The current password is incorrect';
  EST_PASS_LENGTH          = 'The new password must be between 3 and 15 characters';

  // User Configuration
  EST_USER_EXISTS         = 'The specified individual already has a user account.  '+
                            'To change the access rights for this user, please edit '+
                            'the existing account.';
  EST_CURRENT_USER_EDIT   = 'You cannot edit your own user account.';
  EST_CURRENT_USER_DELETE = 'You cannot delete your own user account.';

  EST_LOGO_MISSING = 'Application cannot start.'#10#13'The logo bitmap is not present '
                  + 'in the installation directory : ';
  EST_EXEFILE_MISSING = 'Application cannot start.'#10#13'Main application file problem.';
  EST_OUT_OF_RESOURCES = 'Application cannot start.'#10#13'Out of system resources.';
  EST_MAINFILE_PROBLEM = 'Application cannot start.'#10#13'Problem with main application file.';  

//==============================================================================
implementation

// Nothing to implement

//==============================================================================
end.

