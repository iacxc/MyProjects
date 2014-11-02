package com.caichengxin.count24;

import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;


public class Count24Activity extends ActionBarActivity {

    private Button mButtonGenerate, mButtonGetAnswer;
    private TextView mTextAnswer;
    private EditText mEditNumbers;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_count24);

        mEditNumbers = (EditText)findViewById(R.id.edit_numbers);
        mTextAnswer = (TextView)findViewById(R.id.text_answer);
        mButtonGenerate = (Button)findViewById(R.id.button_generate);
        mButtonGenerate.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {

                int[] numbers = new int[4];
                for(int i=0; i<4; i++) {
                    numbers[i] = (int)(Math.random()*3);
                }

                mEditNumbers.setText(String.valueOf(numbers[0])
                        + "," + numbers[1]
                        + "," + numbers[2]
                        + "," + numbers[3]);
            }
        });
        mButtonGetAnswer = (Button)findViewById(R.id.button_answer);
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.count24, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();
        if (id == R.id.action_settings) {
            return true;
        }
        return super.onOptionsItemSelected(item);
    }
}
