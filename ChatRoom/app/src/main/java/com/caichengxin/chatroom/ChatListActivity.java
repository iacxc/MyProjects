package com.caichengxin.chatroom;

import android.app.Fragment;


public class ChatListActivity extends SingleFragmentActivity
{
    public static final User ME = new User(0, "CaiChengxin");

    @Override
    protected Fragment createFragment() {
        return new ChatListFragment();
    }

}
