package com.caichengxin.chatroom;

import android.content.Context;
import android.util.Log;

import java.util.ArrayList;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class ChatLab
{
    private static final String TAG = "chatroom.ChatLab";

    private ArrayList<Chat> mChats;

    private static ChatLab sChatLab;
    private Context mAppContext;

    private void create()
    {
        mChats = new ArrayList<Chat>();

        for (int i=0; i< 10; i++) {
            User owner = UserLab.get().getRandomUser();
            User user = UserLab.get().getRandomUser();

            Log.d(TAG, "Generating owner:" + owner + ", user:" + user);

            if (owner.equals(ChatListActivity.ME)
                    || user.equals(ChatListActivity.ME)) {

                Chat chat = new Chat(owner);
                chat.addUser(user);

                if (owner.equals(ChatListActivity.ME)) {
                    chat.setName(user.getName());
                } else {
                    chat.setName(owner.getName());
                }

                mChats.add(chat);
            }
        }
    }


    private ChatLab(Context appContext) {
        mAppContext = appContext;

        create();

    }

    public static ChatLab get(Context c) {
        if (sChatLab == null) {
            sChatLab = new ChatLab(c.getApplicationContext());
        }
        return sChatLab;
    }

    public Chat getChat(UUID chatId) {
        for (Chat chat : mChats) {
            if (chat.getId().equals(chatId))
                return chat;
        }
        return null;
    }

    public ArrayList<Chat> getChats() {
        return mChats;
    }
}
