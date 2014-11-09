package com.caichengxin.count24;

import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TableLayout;
import android.widget.TableRow;
import android.widget.TextView;
import android.widget.Toast;

import java.util.Date;
import java.util.List;


public class Count24Activity extends ActionBarActivity {

    private Date mStartDate;
    private TextView mTextAnswer;
    private EditText[] mEditNumbers;



    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_count24);

        TableLayout tableLayout = (TableLayout)findViewById(R.id.numbers_tableLayout);

        TableRow numberRow = (TableRow)tableLayout.getChildAt(0);

        mEditNumbers = new EditText[]{(EditText)numberRow.getChildAt(0),
                (EditText)numberRow.getChildAt(1),
                (EditText)numberRow.getChildAt(2),
                (EditText)numberRow.getChildAt(3)};

        mTextAnswer = (TextView)findViewById(R.id.text_answer);
        Button buttonGenerate = (Button)findViewById(R.id.button_generate);
        buttonGenerate.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                for(int i=0; i<4; i++) {
                    mEditNumbers[i].setText(
                            String.valueOf((int)(Math.random()*10) + 1));
                }
                mStartDate = new Date();
                mTextAnswer.setText("");

            }
        });

        Button buttonGetAnswer = (Button)findViewById(R.id.button_answer);
        buttonGetAnswer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                List<String> resultList = Calculate.easyCount(
                        new int[] {Integer.valueOf(mEditNumbers[0].getText().toString()),
                                   Integer.valueOf(mEditNumbers[1].getText().toString()),
                                   Integer.valueOf(mEditNumbers[2].getText().toString()),
                                   Integer.valueOf(mEditNumbers[3].getText().toString())});

                if (resultList.size() > 0) {
                    Date now = new Date();
                    String format_answer = getResources().getString(R.string.format_answer);
                    mTextAnswer.setText(String.format(format_answer,
                            resultList.get(0),
                            now.getTime() - mStartDate.getTime()));
                }
                else {
                    Toast.makeText(Count24Activity.this, R.string.no_answer,
                            Toast.LENGTH_SHORT).show();
                    mTextAnswer.setText("");
                }
            }
        });
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
        switch (item.getItemId()) {
            case R.id.new_game:
                for(int i=0; i< 4; i++)
                    mEditNumbers[i].setText("");

                mTextAnswer.setText("");
                return true;
            case  R.id.action_settings:
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
}
