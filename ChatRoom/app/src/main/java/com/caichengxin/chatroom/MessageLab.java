package com.caichengxin.chatroom;

import java.util.ArrayList;


public class MessageLab {

    private final ArrayList<Message> mChatMessageList;

    private static MessageLab sMessageLab;

    private MessageLab() {
        mChatMessageList = new ArrayList<Message>();

    }

    public static MessageLab get() {
        if (sMessageLab == null) {
            sMessageLab = new MessageLab();
        }
        return sMessageLab;
    }

    public ArrayList<Message> getChatMessageList() {
        return mChatMessageList;
    }

    public void init(Chat chat) {
        mChatMessageList.clear();

        ArrayList<User> userList = chat.getUserList();

        for (int i = 0; i < 10; i++) {
            Message msg = new Message(chat.getId(),
                    userList.get((int)(Math.random() * userList.size())),
                    "Message # " + i);

            mChatMessageList.add(msg);
        }
    }


}
