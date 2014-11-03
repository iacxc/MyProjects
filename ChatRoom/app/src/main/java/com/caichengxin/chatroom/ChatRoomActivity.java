package com.caichengxin.chatroom;

import android.os.Bundle;
import android.support.v4.app.Fragment;

import java.util.UUID;


public class ChatRoomActivity extends SingleFragmentActivity {


    @Override
    protected Fragment createFragment() {
        UUID roomId = (UUID)getIntent()
                .getSerializableExtra(ChatRoomFragment.EXTRA_ID);

        return ChatRoomFragment.newInstance(roomId);

    }

}
