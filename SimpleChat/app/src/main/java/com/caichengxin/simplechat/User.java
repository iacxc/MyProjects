package com.caichengxin.simplechat;

/**
 * Created by caiche on 2014/11/3.
 */
public class User
{
    private int mId;
    private String mName;

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


    public boolean equals(User user) {
        return this.mId == user.getId();
    }

    @Override
    public String toString() { return mName;}
}
