package com.caichengxin.chatroom;

import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/2.
 */
public class RoomFragment extends Fragment
{
    private static final String TAG = "RoomFragment";
    
    private ListView mLvChatRooms;
    private ArrayList<Room> mRooms;


    @Override
    public void onCreate(Bundle savedInstance) {
        super.onCreate(savedInstance);

        getActivity().setTitle(R.string.chatroom_title);
        mRooms = RoomLab.get(getActivity()).getRooms();

    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState)
    {
        View view = inflater.inflate(R.layout.fragment_chat_list,
                container, false);

        ChatRoomAdapter adapter = new ChatRoomAdapter(mRooms);
        mLvChatRooms = (ListView)view.findViewById(R.id.list_chat);
        mLvChatRooms.setAdapter(adapter);

        mLvChatRooms.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

                Room room = (Room)(parent.getAdapter()).getItem(position);

                Log.d(TAG, room.getTitle() + " was clicked");

                Intent i = new Intent(getActivity(), MessageActivity.class);
                i.putExtra(MessageFragment.EXTRA_TITLE, room.getTitle());

                startActivityForResult(i, 0);
            }
        });

        return view;
    }


    private class ChatRoomAdapter extends ArrayAdapter<Room>
    {

        public ChatRoomAdapter(ArrayList<Room> rooms) {
            super(getActivity(), android.R.layout.simple_list_item_1, rooms);
        }
    }
}
