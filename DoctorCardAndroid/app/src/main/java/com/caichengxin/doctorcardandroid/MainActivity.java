package com.caichengxin.doctorcardandroid;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.accessibility.AccessibilityManager;
import android.widget.Toast;


public class MainActivity extends Activity
        implements LoginFragment.Callbacks, IndexFragment.OnIndexPressedListener
{
    private static final String TAG = "doctorcardandroid.MainActivity";

    private  static final String DIALOG_LOGIN = "login";

    //this should be the user who just login, but now just a faked one
    private User Me;

    private ChatListFragment mFragmentChatlist = null;
    private FriendFragment mFragmentFriend = null;
    private DiscoverFragment mFragmentDiscover = null;
    private MyFragment mFragmentMyself = null;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        login();
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_item_login:
                login();
                return true;
            case R.id.action_settings:
                Log.i(TAG, "action settings menu item clicked");
                return true;
        }

        return super.onOptionsItemSelected(item);
    }


    //interfaces
    public void onChatButtonPressed()
    {
        if (!checkLogin())   return;

        if (mFragmentChatlist == null)
            mFragmentChatlist =  ChatListFragment.newInstance(Me);

        showFragment( mFragmentChatlist );
    }

    public void onFriendButtonPressed()
    {
        if (!checkLogin())    return;

        if (mFragmentFriend == null)
            mFragmentFriend = FriendFragment.newInstance(Me);

        showFragment( mFragmentFriend);
    }

    public void onDiscoverButtonPressed()
    {
        if (!checkLogin())   return;

        if (mFragmentDiscover == null)
            mFragmentDiscover = new DiscoverFragment();

        showFragment( mFragmentDiscover );
    }

    public void onMyselfButtonPressed()
    {
        if (!checkLogin())  return;

        if (mFragmentMyself == null)
            mFragmentMyself = new MyFragment();

        showFragment( mFragmentMyself );
    }

    public void onLoginOkButtonClick (User user) {
        Me = user;
        onChatButtonPressed();
    }


    //utilities
    private void login()
    {
        if (Me == null) {
            FragmentManager fm = getFragmentManager();
            LoginFragment login = new LoginFragment();
            login.show(fm, DIALOG_LOGIN);
        }
    }

    private boolean checkLogin() {
        if (Me == null) {
            Toast.makeText(this,
                    R.string.please_login, Toast.LENGTH_LONG).show();
            return false;
        }
        else
            return true;
    }

    private void showFragment(Fragment fragment) {
        getFragmentManager().beginTransaction()
                .replace(R.id.fragment_container, fragment)
                .commit();
    }


}
