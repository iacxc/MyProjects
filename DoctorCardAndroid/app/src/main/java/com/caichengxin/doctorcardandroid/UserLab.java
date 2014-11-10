package com.caichengxin.doctorcardandroid;

import android.util.Log;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/9.
 */
public class UserLab {
    private static final String TAG = "doctorcardandroid.UserLab";

    //for debug purpose
    private static final User[] sUsers = new User[]{
            new User(0, "cxcai", "CaiChengxin"),
            new User(10, "wlh", "WangHailong"),
            new User(11, "zhu", "ZhuYadong"),
            new User(12, "nj", "NiJun"),
            new User(100, "user1"),
            new User(101, "user2"),
            new User(102, "user3")};

    private static UserLab sUserLab;
    private ArrayList<User> mUserList;

    private UserLab() {
        mUserList = new ArrayList<User>();

        //for debug purpose
        for (int i = 0; i < sUsers.length; i++)
            mUserList.add(sUsers[i]);

    }

    public static UserLab get() {
        if (sUserLab == null)
            sUserLab = new UserLab();

        return sUserLab;
    }

    public User findUserById(long userId) {
        Log.d(TAG, "searching user #" + userId);

        for (User user : mUserList) {
            if (user.getId() == userId)
                return user;
        }

        //use rest api to search the user
        boolean found = false;
        if (found) {
            long foundId = 1000;
            String foundName = "nuser";
            User user = new User(foundId, foundName);
            user.setDisplayName("New User");
            mUserList.add(user);
            return user;
        }

        return null;
    }

    public User findUserByName(String name) {
        Log.d(TAG, "searching user :" + name);

        for (User user : mUserList) {
            if (user.getName() == name)
                return user;
        }

        //use rest api to search the user
        boolean found = false;
        if (found) {
            long foundId = 1000;
            String foundName = "nuser";
            User user = new User(foundId, foundName);
            user.setDisplayName("New User");
            mUserList.add(user);
            return user;
        }

        return null;
    }

    public User getRandomUser() {
        int index = (int)(Math.random() * sUsers.length);

        return sUsers[index];
    }
}
