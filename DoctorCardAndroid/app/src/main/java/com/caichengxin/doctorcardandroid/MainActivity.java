package com.caichengxin.doctorcardandroid;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;


public class MainActivity extends Activity
        implements IndexFragment.OnIndexPressedListener
{
    private static final String TAG = "doctorcardandroid.MainActivity";

    //this should be the user who just login, but now just a faked one
    public static final User ME = UserLab.get().findUserById(0);

    private ChatListFragment mFragmentChatlist = null;
    private FriendFragment mFragmentFriend = null;
    private DiscoverFragment mFragmentDiscover = null;
    private MyFragment mFragmentMy = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        if (savedInstanceState != null)
            return;

        onChatButtonPressed();

    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            Log.i(TAG, "action settings menu item clicked");
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    private void showFragment(Fragment fragment) {
        getFragmentManager().beginTransaction()
                .replace(R.id.fragment_container, fragment)
                .commit();
    }

    public void onChatButtonPressed()
    {
        if (mFragmentChatlist == null)
            mFragmentChatlist = new ChatListFragment();

        showFragment( mFragmentChatlist );
    }

    public void onFriendButtonPressed()
    {
        if (mFragmentFriend == null)
            mFragmentFriend = new FriendFragment();

        showFragment( mFragmentFriend);
    }

    public void onDiscoverButtonPressed()
    {
        if (mFragmentDiscover == null)
            mFragmentDiscover = new DiscoverFragment();

        showFragment( mFragmentDiscover );
    }
    public void onMyselfButtonPressed()
    {
        if (mFragmentMy == null)
            mFragmentMy = new MyFragment();

        showFragment( mFragmentMy );
    }
}
