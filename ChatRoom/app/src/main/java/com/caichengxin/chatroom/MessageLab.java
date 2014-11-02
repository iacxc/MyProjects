package com.caichengxin.chatroom;

import android.content.Context;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/2.
 */
public class MessageLab {

    private ArrayList<Message> mChatMessageList;

    private static MessageLab sMessageLab;
    private Context mAppContext;

    private void loadChatMessages()
    {
        mChatMessageList = new ArrayList<Message>();

        for (int i=0; i< 5; i++) {
            Message message = new Message("hello", "message #" + i);

            mChatMessageList.add(message);
        }
    }


    private MessageLab(Context appContext) {
        mAppContext = appContext;

        loadChatMessages();

    }

    public static MessageLab get(Context c) {
        if (sMessageLab == null) {
            sMessageLab = new MessageLab(c.getApplicationContext());
        }
        return sMessageLab;
    }

    public ArrayList<Message> getChatMessageList() {
        return mChatMessageList;
    }
}
