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


    private MessageLab(Context appContext) {
        mAppContext = appContext;
        mChatMessageList = new ArrayList<Message>();

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

    public void addMessage(Message msg) {
        mChatMessageList.add(msg);
    }

    public void init(Chat chat) {
        mChatMessageList.clear();

        ArrayList<User> userList = chat.getUserList();

        for (int i = 0; i < 10; i++) {
            Message msg = new Message(chat.getId(),
                    userList.get((int)(Math.random() * userList.size())),
                    "Message # " + i);

            addMessage(msg);
        }
    }


}
