package com.caichengxin.chatroom;


public class User
{
    private final int mId;
    private final String mName;

    public User(int id, String name)
    {
        mId = id;
        mName = name;
    }

    public int getId() {
        return mId;
    }

    public String getName() {
        return mName;
    }

    public boolean equals(User user) {   return mId == user.getId();    }

    @Override
    public String toString() { return mName;}
}
