package com.caichengxin.doctorcardandroid;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.ComponentCallbacks;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;
import android.widget.Toast;

/**
 * Created by caiche on 2014/11/11.
 */
public class LoginFragment extends DialogFragment
{
    private Callbacks mCallbacks;

    public interface Callbacks {
        void onLoginOkButtonClick(User user);
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        View view = getActivity().getLayoutInflater()
                .inflate(R.layout.fragment_login, null);

        final EditText editUsername =
                (EditText)view.findViewById(R.id.edit_username);

        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        builder.setView(view)
                .setTitle(R.string.login_title)
                .setCancelable(false)
                .setPositiveButton(android.R.string.ok,
                    new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which)
                        {
                            String username = editUsername.getText().toString();
                            User user = UserLab.get().findUserByName(username);
                            if (user == null) {
                                String format = getResources()
                                        .getString(R.string.invalid_user);
                                Toast.makeText(getActivity(),
                                        String.format(format, username),
                                        Toast.LENGTH_LONG).show();
                            }
                            else
                                mCallbacks.onLoginOkButtonClick(user);
                        }
                    })
                .setNegativeButton(android.R.string.cancel, null);

        return builder.create();
    }

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);

        try {
            mCallbacks = (Callbacks) activity;
        }
        catch (Exception e) {
            throw new ClassCastException(activity.toString()
                + "must implement LoginFragment.Callbacks");
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();

        mCallbacks = null;
    }

}
