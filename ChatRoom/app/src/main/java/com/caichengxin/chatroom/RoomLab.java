package com.caichengxin.chatroom;

import android.content.Context;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/2.
 */
public class RoomLab
{
    private ArrayList<Room> mRooms;

    private static RoomLab sRoomLab;
    private Context mAppContext;

    private void loadChatRooms()
    {
        mRooms = new ArrayList<Room>();

        for (int i=0; i< 5; i++) {
            Room room = new Room();

            room.setTitle("Room # " + i);

            mRooms.add(room);
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

    public ArrayList<Room> getRooms() {
        return mRooms;
    }
}
