package com.caichengxin.chatroom;

import android.support.v4.app.Fragment;

import java.util.UUID;


public class ChatActivity extends SingleFragmentActivity {


    @Override
    protected Fragment createFragment() {
        UUID roomId = (UUID)getIntent()
                .getSerializableExtra(ChatFragment.EXTRA_ID);

        return ChatFragment.newInstance(roomId);

    }

}
