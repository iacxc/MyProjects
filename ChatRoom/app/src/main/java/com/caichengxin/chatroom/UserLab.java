package com.caichengxin.chatroom;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/4.
 */
public class UserLab {
    private static final String[] sNames =
            new String[]{"CaiChengxin",
                    "WangHailong",
                    "ZhuYadong",
                    "NiJun",
                    "User1",
                    "User2",
                    "User3",
                    "User4"
            };

    private ArrayList<User> mUsers;

    private static UserLab sUserLab;

    private void createUsers()
    {
        mUsers = new ArrayList<User>();

        for (int i=0; i< sNames.length; i++) {
            User user = new User(i, sNames[i]);
            mUsers.add(user);
        }
    }


    private UserLab() {
        createUsers();
    }

    public static UserLab get() {
        if (sUserLab == null) {
            sUserLab = new UserLab();
        }
        return sUserLab;
    }

    public User findUserById(int userId) {
        for (User user : mUsers) {
            if (user.getId() == userId)
                return user;
        }
        return null;
    }

    public User findUserByName(String name) {
        for (User user : mUsers) {
            if (user.getName().equals(name))
                return user;
        }
        return null;
    }

    public User getRandomUser() {
        int index = (int)(Math.random() * mUsers.size());

        return mUsers.get(index);
    }

    public ArrayList<User> getUsers() {
        return mUsers;
    }
}
