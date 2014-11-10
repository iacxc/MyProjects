package com.caichengxin.doctorcardandroid;


public class User
{
    private final long mId;
    private final String mName;
    private  String mDisplayName;

    public User(long id, String name)
    {
        mId = id;
        mName = name;
    }

    public User(long id, String name, String displayName)
    {
        this(id, name);
        mDisplayName = displayName;
    }

    public long getId() {
        return mId;
    }

    public String getName() {
        return mName;
    }

    public String getDisplayName() {
        return mDisplayName;
    }

    public void setDisplayName(String displayName) {
        mDisplayName = displayName;
    }

    public boolean equals(User user) {   return mId == user.getId();    }

    public String toString() {
        if (mDisplayName != null)
            return mDisplayName;
        else
            return mName;
    }
}
