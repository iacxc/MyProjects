package com.caichengxin.simplechat;

import android.content.Intent;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import java.util.ArrayList;


public class ChatListActivity extends ActionBarActivity {
    private static final String TAG = "simplechat.ChatListActivity";
    public static final User ME = new User(0, "CaiChengxin");

    ListView mLvChats;
    private ArrayList<Chat> mChats;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.chat_list);

        mChats = ChatLab.get(this).getChats();

        ChatAdapter adapter = new ChatAdapter(mChats);

        mLvChats = (ListView)findViewById(R.id.list_chat);
        mLvChats.setAdapter(adapter);

        mLvChats.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view,
                                    int position, long id) {

                Chat chat = (Chat)parent.getAdapter().getItem(position);
                Log.i(TAG, "Owned by " + chat.getOwner()
                        + ", Users:" + chat.getUserList().get(1));

                Intent i = new Intent(ChatListActivity.this, ChatActivity.class);
                i.putExtra(ChatActivity.EXTRA_ID, chat.getId());

                startActivityForResult(i, 0);

            }
        });
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_chat_list, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        switch (item.getItemId()) {
            case R.id.new_chat:
                Log.i(TAG, "New Chat");
                return true;
            case R.id.action_settings:
                Log.i(TAG, "Settings");
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    private class ChatAdapter extends ArrayAdapter<Chat>
    {
        public ChatAdapter(ArrayList<Chat> chats) {
            super(ChatListActivity.this,
                    android.R.layout.simple_list_item_1, chats);
        }
    }
}
