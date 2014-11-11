package com.caichengxin.doctorcardandroid;


import android.content.Intent;
import android.os.Bundle;
import android.app.Fragment;
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


public class ChatListFragment extends Fragment {

    private static final String TAG = "doctorcardandroid.ChatListFragment";
    public static final String EXTRA_OWNER_ID =
            "doctorcardandroid.ChatListFragment.owner_id";

    private User mOwner;

    private ArrayList<Chat> mChatList;
    private ListView mLvChats;


    public static ChatListFragment newInstance(User owner) {

        Bundle args = new Bundle();
        args.putLong(EXTRA_OWNER_ID, owner.getId());

        ChatListFragment fragment = new ChatListFragment();
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
            String title = getResources().getString(R.string.chat_title);
            getActivity().setTitle(title + "(" + mOwner.toString() + ")");
            ChatLab.get().loadChatList(mOwner);
            mChatList = ChatLab.get().getChatList();
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View view = inflater.inflate(R.layout.fragment_chatlist, container, false);

        mLvChats = (ListView)view.findViewById(R.id.list_chats);

        final ChatAdapter chatAadapter = new ChatAdapter(mChatList);
        mLvChats.setAdapter(chatAadapter);
        mLvChats.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                Chat chat = chatAadapter.getItem(position);

                startChat(chat);
            }
        });

        return view;
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        super.onCreateOptionsMenu(menu, inflater);

        inflater.inflate(R.menu.menu_chat_list, menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_item_add_chat:
                Toast.makeText(getActivity(),
                        R.string.menu_add_chat, Toast.LENGTH_LONG).show();
                Chat chat = new Chat((int)(Math.random() * 1000));
                chat.setName(UserLab.get().getRandomUser().getName());

                ChatLab.get().addChat(chat);

                ((ChatAdapter)mLvChats.getAdapter()).notifyDataSetChanged();

                startChat(chat);
                return true;

            case R.id.action_settings:
                Toast.makeText(getActivity(),
                        R.string.action_settings, Toast.LENGTH_LONG).show();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

     private void startChat(Chat chat) {
        Intent i = new Intent(getActivity(), ChatActivity.class);

        i.putExtra(ChatActivity.EXTRA_ID, chat.getId());

        startActivity(i);
    }

    private class ChatAdapter extends ArrayAdapter<Chat>
    {
        public ChatAdapter(ArrayList<Chat> chats) {
            super(getActivity(), android.R.layout.simple_list_item_1, chats);
        }
    }
}
