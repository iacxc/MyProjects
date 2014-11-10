package com.caichengxin.doctorcardandroid;

import android.os.Bundle;
import android.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import java.util.ArrayList;


public class FriendFragment extends Fragment {

    private static final String TAG = "doctorcardandroid.FriendFragment";

    private ArrayList<User> mFriends;
    private ListView mLvFriends;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
        setRetainInstance(true);

        loadFriends(MainActivity.ME);

    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View view = inflater.inflate(R.layout.fragment_friend, container, false);

        ListView mLvFriends = (ListView)view.findViewById(R.id.list_friends);

        ArrayAdapter<User> adapter = new ArrayAdapter<User>(getActivity(),
                android.R.layout.simple_list_item_1, mFriends);
        mLvFriends.setAdapter(adapter);

        return view;
    }

    private void loadFriends(User myself) {
        if (mFriends != null)
            return;

        //use rest call to get all of my friends
        //currently, only insert some faked people
        Log.d(TAG, "loading friends...");

        mFriends = new ArrayList<User>();

        //use rest api to get the user id of all my friends
        //for debug purpose
        UserLab userLab = UserLab.get();
        mFriends.add(userLab.findUserById(10));
        mFriends.add(userLab.findUserById(11));
        mFriends.add(userLab.findUserById(12));
    }

}
