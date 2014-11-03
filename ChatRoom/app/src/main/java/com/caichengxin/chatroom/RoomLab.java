package com.caichengxin.chatroom;

import android.content.Context;

import java.util.ArrayList;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class RoomLab
{
    private ArrayList<ChatRoom> mChatRooms;

    private static RoomLab sRoomLab;
    private Context mAppContext;

    private void loadChatRooms()
    {
        mChatRooms = new ArrayList<ChatRoom>();

        for (int i=0; i< 5; i++) {
            ChatRoom room = new ChatRoom();

            room.setName("Room # " + i);
            mChatRooms.add(room);
        }
    }


    private RoomLab(Context appContext) {
        mAppContext = appContext;

        loadChatRooms();

    }

    public static RoomLab get(Context c) {
        if (sRoomLab == null) {
            sRoomLab = new RoomLab(c.getApplicationContext());
        }
        return sRoomLab;
    }

    public ChatRoom getRoom(UUID roomId) {
        for (ChatRoom room : mChatRooms) {
            if (room.getId().equals(roomId))
                return room;
        }
        return null;
    }
    public ArrayList<ChatRoom> getChatRooms() {
        return mChatRooms;
    }
}
