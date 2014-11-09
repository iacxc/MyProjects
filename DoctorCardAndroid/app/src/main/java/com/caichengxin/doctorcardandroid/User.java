package com.caichengxin.doctorcardandroid;


public class User
{
    private final long mId;
    private final String mName;

    public User(long id, String name)
    {
        mId = id;
        mName = name;
    }

    public long getId() {
        return mId;
    }

    public String getName() {
        return mName;
    }

    public boolean equals(User user) {   return mId == user.getId();    }

    public String toString() { return mName;}
}
