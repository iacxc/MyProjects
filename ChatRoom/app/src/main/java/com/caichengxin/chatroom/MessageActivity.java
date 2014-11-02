package com.caichengxin.chatroom;

import android.support.v4.app.Fragment;


public class MessageActivity extends SingleFragmentActivity {


    @Override
    protected Fragment createFragment() {
        String chatTitle = (String)getIntent()
                .getSerializableExtra(MessageFragment.EXTRA_TITLE);

        return MessageFragment.newInstance(chatTitle);

    }
}
