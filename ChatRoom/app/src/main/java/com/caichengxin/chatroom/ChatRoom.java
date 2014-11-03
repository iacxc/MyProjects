package com.caichengxin.chatroom;

import java.util.ArrayList;
import java.util.Date;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class ChatRoom {
    UUID mId;
    String mName;
    UUID mOwnerId;
    String mOwnerName;
    Date mDate;
    String mLastMessageText;
    ArrayList<UUID> mUserList;

    public ChatRoom()
    {
        mId = UUID.randomUUID();
        mDate = new Date();
    }

    public ChatRoom(UUID id) {
        mId = id;
        mDate = new Date();
    }

    public UUID getId() {
        return mId;
    }

    public String getName() {
        return mName;
    }

    public void setName(String name) {
        mName = name;
    }

    public UUID getOwnerId() {
        return mOwnerId;
    }

    public void setOwnerId(UUID ownerId) {
        mOwnerId = ownerId;
    }

    public String getOwnerName() {
        return mOwnerName;
    }

    public void setOwnerName(String ownerName) {
        mOwnerName = ownerName;
    }

    public Date getDate() {
        return mDate;
    }

    public void setDate(Date date) {
        mDate = date;
    }

    public String getLastMessageText() {
        return mLastMessageText;
    }

    public void setLastMessageText(String lastMessageText) {
        mLastMessageText = lastMessageText;
    }

    public ArrayList<UUID> getUserList() {
        return mUserList;
    }

    public void setUserList(ArrayList<UUID> userList) {
        mUserList = userList;
    }

    public String toString() { return mName; }
}
