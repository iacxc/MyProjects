package com.caichengxin.doctorcardandroid;

import android.util.Log;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/9.
 */
public class ChatLab {
    private static final String TAG = "doctorcardandroid.ChatLab";

    private static ChatLab sChatLab;
    private ArrayList<Chat> mChatList;

    private ChatLab() {
        mChatList = new ArrayList<Chat>();
    }

    public static ChatLab get() {
        if (sChatLab == null)
            sChatLab = new ChatLab();

        return sChatLab;
    }

    public void loadChatList(User myself) {
        //load all the chat related with myself

        Log.d(TAG, "loading chats for " + myself + " ...");

        mChatList.clear();
        for (long i=0; i< 15; i++) {
            Chat chat = new Chat(i);
            chat.setName("Chat # " + i);

            mChatList.add(chat);
        }
    }

    public ArrayList<Chat> getChatList() {
        return mChatList;
    }

    public void addChat(Chat chat) {
        mChatList.add(0, chat);
    }

    public Chat findChatById(long chatId) {
        for (Chat chat : mChatList) {
            if (chat.getId() == chatId)
                return chat;
        }

        return null;
    }
}
