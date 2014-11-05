package com.caichengxin.chatroom;

import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Toast;

import java.util.ArrayList;

/**
 * Created by caiche on 2014/11/2.
 */
public class ChatListFragment extends Fragment
{
    private static final String TAG = "chatroom.ChatListFragment";

    private ListView mLvChatRooms;
    private ArrayList<Chat> mChats;


    private void startChat(Chat chat) {
        Intent i = new Intent(getActivity(), ChatActivity.class);
        i.putExtra(ChatFragment.EXTRA_ID, chat.getId());

        startActivity(i);
    }


    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setHasOptionsMenu(true);

        getActivity().setTitle(R.string.chatroom_title);
        mChats = ChatLab.get(getActivity()).getChats();
    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState)
    {
        View view = inflater.inflate(R.layout.fragment_chat_list,
                container, false);

        ChatListAdapter adapter = new ChatListAdapter(mChats);
        mLvChatRooms = (ListView)view.findViewById(R.id.list_chat);
        mLvChatRooms.setAdapter(adapter);

        mLvChatRooms.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

                Chat chat = (Chat)(parent.getAdapter()).getItem(position);

                startChat(chat);
            }
        });

        return view;
    }


    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
    {
        super.onCreateOptionsMenu(menu, inflater);
        inflater.inflate(R.menu.room_op, menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.new_chat:
                Toast.makeText(getActivity(),
                        R.string.new_chat, Toast.LENGTH_LONG).show();

                Chat chat = new Chat(ChatListActivity.ME);

                User user= UserLab.get().getRandomUser();
                chat.addUser(user);
                chat.setName(user.getName());

                mChats.add(chat);

                ((ChatListAdapter)mLvChatRooms.getAdapter())
                        .notifyDataSetChanged();

                startChat(chat);

                return true;
            case R.id.action_settings:
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    private class ChatListAdapter extends ArrayAdapter<Chat>
    {

        public ChatListAdapter(ArrayList<Chat> chats) {
            super(getActivity(), android.R.layout.simple_list_item_1, chats);
        }
    }
}
