package com.caichengxin.chatroom;

import java.util.ArrayList;
import java.util.Date;
import java.util.UUID;

/**
 * Created by caiche on 2014/11/2.
 */
public class Chat {

    private UUID mId;
    private String mName;
    private Date mDate;
    private String mLastMessageText;
    private ArrayList<User> mUserList;

    public Chat(User owner)
    {
        mId = UUID.randomUUID();
        mDate = new Date();

        mUserList = new ArrayList<User>();
        mUserList.add(owner);
    }


    public UUID getId() {        return mId;    }

    public String getName() {        return mName;    }

    public void setName(String name) {        mName = name;    }

    public Date getDate() {        return mDate;    }

    public void setDate(Date date) {        mDate = date;    }

    public String getLastMessageText() { return mLastMessageText;    }

    public void setLastMessageText(String lastMessageText) {
        mLastMessageText = lastMessageText;
    }

    public User getOwner() { return mUserList.get(0); }

    public ArrayList<User> getUserList() {  return mUserList;   }

    public void addUser(User user) {   mUserList.add(user);  }

    public String toString() { return mName; }
}
