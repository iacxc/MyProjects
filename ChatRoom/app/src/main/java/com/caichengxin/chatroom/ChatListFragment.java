package com.caichengxin.chatroom;

import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.ContextMenu;
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


public class ChatListFragment extends Fragment
{
    private static final String TAG = "chatroom.ChatListFragment";

    private ListView mLvChats;
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
        mChats = ChatLab.get().getChats();

        setRetainInstance(true);
    }


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState)
    {
        View view = inflater.inflate(R.layout.fragment_chat_list,
                container, false);

        ChatListAdapter adapter = new ChatListAdapter(mChats);
        mLvChats = (ListView)view.findViewById(R.id.list_chat);
        mLvChats.setAdapter(adapter);

        mLvChats.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

                Chat chat = (Chat)(parent.getAdapter()).getItem(position);

                startChat(chat);
            }
        });

        registerForContextMenu(mLvChats);
        return view;
    }


    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater)
    {
        super.onCreateOptionsMenu(menu, inflater);
        inflater.inflate(R.menu.fragment_chat_list, menu);
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_item_new_chat:
                Toast.makeText(getActivity(),
                        R.string.new_chat, Toast.LENGTH_LONG).show();

                Chat chat = new Chat(ChatListActivity.ME);

                User user= UserLab.get().getRandomUser();
                chat.addUser(user);
                chat.setName(user.getName());

                ChatLab.get().addChat(chat);

                ((ChatListAdapter)mLvChats.getAdapter())
                        .notifyDataSetChanged();

                startChat(chat);

                return true;
            case R.id.action_settings:
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v,
                                    ContextMenu.ContextMenuInfo menuInfo)
    {
        getActivity().getMenuInflater()
                .inflate(R.menu.chat_list_item_context, menu);
    }

    @Override
    public boolean onContextItemSelected(MenuItem item) {
        AdapterView.AdapterContextMenuInfo info =
                (AdapterView.AdapterContextMenuInfo)item.getMenuInfo();
        ChatListAdapter adapter = (ChatListAdapter)mLvChats.getAdapter();
        Chat chat = adapter.getItem(info.position);

        switch (item.getItemId()) {
            case R.id.menu_item_delete_chat:
                ChatLab.get().deleteChat(chat);
                adapter.notifyDataSetChanged();

                return true;

        }
        return super.onContextItemSelected(item);
    }
    private class ChatListAdapter extends ArrayAdapter<Chat>
    {

        public ChatListAdapter(ArrayList<Chat> chats) {
            super(getActivity(), android.R.layout.simple_list_item_1, chats);
        }
    }
}
