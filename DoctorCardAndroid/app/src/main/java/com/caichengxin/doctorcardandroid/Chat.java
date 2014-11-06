package com.caichengxin.doctorcardandroid;

import android.graphics.Point;

import java.util.Date;

public class Chat {

    private final long mId;
    private String mName;
    private String mUuid;
    private int mCapacity;
    private Point mLocation;
    private Date mCreateDate;
    private Date mUpdateDate;

    public Chat(long id)
    {
        mId = id;
        mCreateDate = new Date();
        mUpdateDate = mCreateDate.clone();
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

    public String getUuid() {
        return mUuid;
    }

    public void setUuid(String uuid) {
        mUuid = uuid;
    }

    public int getCapacity() {
        return mCapacity;
    }

    public void setCapacity(int capacity) {
        mCapacity = capacity;
    }

    public Point getLocation() {
        return mLocation;
    }

    public void setLocation(Point location) {
        mLocation = location;
    }

    public Date getCreateDate() {
        return mCreateDate;
    }

    public void setCreateDate(Date createDate) {
        mCreateDate = createDate;
    }

    public Date getUpdateDate() {
        return mUpdateDate;
    }

    public void setUpdateDate(Date updateDate) {
        mUpdateDate = updateDate;
    }

    public void update() { mUpdateDate = new Date(); }

    public String toString() { return mName; }
}
