package com.caichengxin.chatroom;

import android.support.v4.app.Fragment;


public class RoomListActivity extends SingleFragmentActivity
{

    @Override
    protected Fragment createFragment() {
        return new RoomListFragment();
    }
}
