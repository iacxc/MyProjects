package com.caichengxin.doctorcardandroid;

import android.graphics.Point;

import java.util.Date;

public class Chat {

    private  long mId;
    private String mName;

    public Chat(long id)
    {
        mId = id;
    }

    public long getId() {
        return mId;
    }

    public String getName() {
        return mName;
    }

    public void setName(String name) {
        mName = name;
    }

    public String toString() { return mName; }
}
