package com.caichengxin.chatroom;

import android.support.v4.app.Fragment;


public class ChatListActivity extends SingleFragmentActivity
{
    public static final User ME = new User(0, "CaiChengxin");

    @Override
    protected Fragment createFragment() {
        return new ChatListFragment();
    }

}
