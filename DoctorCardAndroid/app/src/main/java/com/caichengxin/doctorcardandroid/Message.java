package com.caichengxin.doctorcardandroid;

import android.graphics.Point;

import java.util.Date;
import java.util.UUID;


public class Message
{
    private long mId;
    private Byte[]  mMediaContent;
    private String mMediaType;
    private String mMediaIconUrl;
    private Point mLocation;
    private long mAuthorId;
    Date mCreateDate;

    public Message(long id) {
        mId = id;
        mCreateDate = new Date();
    }

    public long getId() {
        return mId;
    }

    public void setId(long id) {
        mId = id;
    }

    public Byte[] getMediaContent() {
        return mMediaContent;
    }

    public void setMediaContent(Byte[] mediaContent) {
        mMediaContent = mediaContent;
    }

    public String getMediaType() {
        return mMediaType;
    }

    public void setMediaType(String mediaType) {
        mMediaType = mediaType;
    }

    public String getMediaIconUrl() {
        return mMediaIconUrl;
    }

    public void setMediaIconUrl(String iconUrl) {
        mMediaIconUrl = iconUrl;
    }

    public Point getLocation() {
        return mLocation;
    }

    public void setLocation(Point location) {
        mLocation = location;
    }

    public long getAuthorId() {
        return mAuthorId;
    }

    public void setAuthorId(long authorId) {
        mAuthorId = authorId;
    }

    public Date getCreateDate() {
        return mCreateDate;
    }

    public void setCreateDate(Date createDate) {
        mCreateDate = createDate;
    }

    public String toString() {
        if (mMediaType.equals("text"))
            return mMediaContent;
        else
            return "<" + mMediaType + ">";
    }

}
