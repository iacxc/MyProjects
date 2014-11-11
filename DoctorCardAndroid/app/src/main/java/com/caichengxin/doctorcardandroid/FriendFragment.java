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
    public static final String EXTRA_OWNER_ID =
            "doctorcardandroid.FriendFragment.owner_id";

    private User mOwner;

    private ArrayList<User> mFriends;
    private ListView mLvFriends;

    public static FriendFragment newInstance(User owner) {

        Bundle args = new Bundle();
        args.putLong(EXTRA_OWNER_ID, owner.getId());

        FriendFragment fragment = new FriendFragment();
        fragment.setArguments(args);

        return fragment;
    }


    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
        setRetainInstance(true);

        if (mOwner == null) {
            long userId = getArguments().getLong(EXTRA_OWNER_ID, -1);
            mOwner = UserLab.get().findUserById(userId);
        }

        if (mOwner != null) {
            String title = getResources().getString(R.string.friend_title);
            getActivity().setTitle(title + "(" + mOwner.toString() + ")");

            if (mFriends == null)
                mFriends = loadFriends(mOwner);
        }
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

    private ArrayList<User> loadFriends(User myself) {
        //use rest call to get all of my friends
        //currently, only insert some faked people
        Log.d(TAG, "loading friends...");

        ArrayList<User> friends = new ArrayList<User>();

        //use rest api to get the user id of all my friends
        //for debug purpose
        UserLab userLab = UserLab.get();
        friends.add(userLab.findUserById(10));
        friends.add(userLab.findUserById(11));
        friends.add(userLab.findUserById(12));

        return friends;
    }

}
