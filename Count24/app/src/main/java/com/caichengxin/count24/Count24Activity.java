package com.caichengxin.count24;

import android.app.Activity;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import java.util.Date;
import java.util.List;
import java.util.Stack;


public class Count24Activity extends Activity {

    private static final String TAG = "Count24Activity";
    // drawable constants
    private final int[][] mDrawables = new int[][]{
        {R.drawable.c1_c, R.drawable.c1_d, R.drawable.c1_h, R.drawable.c1_s},
        {R.drawable.c2_c, R.drawable.c2_d, R.drawable.c2_h, R.drawable.c2_s},
        {R.drawable.c3_c, R.drawable.c3_d, R.drawable.c3_h, R.drawable.c3_s},
        {R.drawable.c4_c, R.drawable.c4_d, R.drawable.c4_h, R.drawable.c4_s},
        {R.drawable.c5_c, R.drawable.c5_d, R.drawable.c5_h, R.drawable.c5_s},
        {R.drawable.c6_c, R.drawable.c6_d, R.drawable.c6_h, R.drawable.c6_s},
        {R.drawable.c7_c, R.drawable.c7_d, R.drawable.c7_h, R.drawable.c7_s},
        {R.drawable.c8_c, R.drawable.c8_d, R.drawable.c8_h, R.drawable.c8_s},
        {R.drawable.c9_c, R.drawable.c9_d, R.drawable.c9_h, R.drawable.c9_s},
        {R.drawable.c10_c, R.drawable.c10_d,
                R.drawable.c10_h, R.drawable.c10_s},
    };

    private final int mBacks[] = new int[] {
            R.drawable.back, R.drawable.back1,
            R.drawable.back2, R.drawable.back3};

    // control widgets
    private ImageView[] mImageCards;
    private EditText[] mEditNumbers;
    private Button mButtonDeal, mButtonInput, mButtonOK, mButtonGetAnswer;
    private LinearLayout mLayoutNumbers, mLayoutInputs;
    private TextView mTextAnswer, mTextInput;

    // class members
    private Date mStartDate;
    private Stack<String> mStackInputs = new Stack<String>();
    private boolean mStarted = false;
    private boolean mAutomatic = true;


    private int pickCardId(int n) {
        int index = (int)(Math.random() * 3);
        return mDrawables[n-1][index];
    }


    public void clearInput() {
        mTextInput.setText("");
        mTextAnswer.setText("");

        if (mAutomatic) {
            mLayoutNumbers.setVisibility(View.INVISIBLE);
            mLayoutInputs.setVisibility(View.VISIBLE);
        }
        else {
            mLayoutNumbers.setVisibility(View.VISIBLE);
            mLayoutInputs.setVisibility(View.INVISIBLE);
        }

        mButtonOK.setVisibility(mStarted ? View.VISIBLE
                : View.INVISIBLE);

        mButtonGetAnswer.setVisibility(mStarted ? View.VISIBLE
                                                : View.INVISIBLE);

        mStackInputs.clear();
    }


    private void updateInput() {
        StringBuilder sb = new StringBuilder();
        for (String item : mStackInputs)
        {
            sb.append(" ");
            sb.append(item);
        }

        mTextInput.setText(sb.toString());
    }


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_count24);

        mLayoutNumbers = (LinearLayout)findViewById(R.id.layout_numbers);
        mLayoutInputs = (LinearLayout)findViewById(R.id.layout_inputs);

        mTextInput = (TextView)findViewById(R.id.text_input);
        mTextAnswer = (TextView)findViewById(R.id.text_answer);

        ImageView image1 = (ImageView)findViewById(R.id.image1);
        ImageView image2 = (ImageView)findViewById(R.id.image2);
        ImageView image3 = (ImageView)findViewById(R.id.image3);
        ImageView image4 = (ImageView)findViewById(R.id.image4);
        mImageCards = new ImageView [] {image1, image2, image3, image4};

        EditText text1 = (EditText)findViewById(R.id.text1);
        EditText text2 = (EditText)findViewById(R.id.text2);
        EditText text3 = (EditText)findViewById(R.id.text3);
        EditText text4 = (EditText)findViewById(R.id.text4);
        mEditNumbers = new EditText[]{text1, text2, text3, text4};

        text1.addTextChangedListener(new CardTextWatcher(image1));
        text2.addTextChangedListener(new CardTextWatcher(image2));
        text3.addTextChangedListener(new CardTextWatcher(image3));
        text4.addTextChangedListener(new CardTextWatcher(image4));

        image1.setOnClickListener(new NumberOnClickListener(text1));
        image2.setOnClickListener(new NumberOnClickListener(text2));
        image3.setOnClickListener(new NumberOnClickListener(text3));
        image4.setOnClickListener(new NumberOnClickListener(text4));

        mButtonDeal = (Button)findViewById(R.id.button_deal);
        mButtonDeal.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                mAutomatic = true;
                mStarted = true;
                clearInput();

                for(int i=0; i<4; i++) {
                    int index = (int)(Math.random()*10);
                    mEditNumbers[i].setText(String.valueOf(index + 1));
                    mImageCards[i].setEnabled(true);
                }

                mStartDate = new Date();
            }
        });

        mButtonInput = (Button)findViewById(R.id.button_input);
        mButtonInput.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mAutomatic = false;
                mStarted = true;
                clearInput();

                int idxBack = (int)(Math.random() * mBacks.length);
                for(int i=0; i<4; i++) {
                    mEditNumbers[i].setText("");
                    mImageCards[i].setImageResource(mBacks[idxBack]);
                }
            }
        });

        mButtonOK = (Button)findViewById(R.id.button_ok);
        mButtonOK.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // check result
                if (mStackInputs.empty())
                    return;

                Date now = new Date();
                String format_time = getResources()
                        .getString(R.string.format_time);
                mTextAnswer.setText(String.format(format_time,
                        (now.getTime() - mStartDate.getTime()) / 1000.0));

                try {
                    int answer = Calculator.calculate(mStackInputs);
                    mStackInputs.push("=");
                    mStackInputs.push(String.valueOf(answer));
                    updateInput();

                    if (answer == 24) {
                        Toast.makeText(Count24Activity.this,
                                R.string.correct,
                                Toast.LENGTH_LONG).show();
                    }
                    else {
                        Toast.makeText(Count24Activity.this,
                                R.string.wrong,
                                Toast.LENGTH_LONG).show();
                    }
                }
                catch (Exception e) {
                    Toast.makeText(Count24Activity.this,
                            R.string.wrong,
                            Toast.LENGTH_LONG).show();
                }
            }
        });

        mButtonGetAnswer = (Button)findViewById(R.id.button_answer);
        mButtonGetAnswer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                // find solution to count24
                for (EditText e : mEditNumbers) {
                    if (e.length() == 0)
                        return;
                }
                List<String> resultList = Calculator.easyCount24(
                    Integer.valueOf(mEditNumbers[0].getText().toString()),
                    Integer.valueOf(mEditNumbers[1].getText().toString()),
                    Integer.valueOf(mEditNumbers[2].getText().toString()),
                    Integer.valueOf(mEditNumbers[3].getText().toString()));

                if (resultList.size() > 0) {
                    mTextAnswer.setText(resultList.get(0));
                }
                else {
                    Toast.makeText(Count24Activity.this, R.string.no_answer,
                            Toast.LENGTH_SHORT).show();
                    mTextAnswer.setText("");
                }
            }
        });

        ImageView imageAdd = (ImageView)findViewById(R.id.image_add);
        imageAdd.setOnClickListener(new OperatorOnClickListener("+"));

        ImageView imageSub = (ImageView)findViewById(R.id.image_sub);
        imageSub.setOnClickListener(new OperatorOnClickListener("-"));

        ImageView imageMul = (ImageView)findViewById(R.id.image_mul);
        imageMul.setOnClickListener(new OperatorOnClickListener("X"));

        ImageView imageDiv = (ImageView)findViewById(R.id.image_div);
        imageDiv.setOnClickListener(new OperatorOnClickListener("/"));

        ImageView imageLeftP = (ImageView)findViewById(R.id.image_leftp);
        imageLeftP.setOnClickListener(new OperatorOnClickListener("("));

        ImageView imageRightP = (ImageView)findViewById(R.id.image_rightp);
        imageRightP.setOnClickListener(new OperatorOnClickListener(")"));

        ImageView imageDel = (ImageView)findViewById(R.id.image_del);
        imageDel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (mStackInputs.size() > 0) {
                    mStackInputs.pop();
                    updateInput();
                }
            }
        });

        ImageView imageClear = (ImageView)findViewById(R.id.image_clear);
        imageClear.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mStackInputs.clear();
                updateInput();
            }
        });

        clearInput();

        mLayoutNumbers.setVisibility(View.INVISIBLE);
        mLayoutInputs.setVisibility(View.INVISIBLE);

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
            case  R.id.action_settings:
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    private class CardTextWatcher implements TextWatcher {
        private ImageView mImage;
        public CardTextWatcher(ImageView image) {
            mImage = image;
        }

        @Override
        public void beforeTextChanged(CharSequence s,
                                      int start, int count, int after) { }

        @Override
        public void onTextChanged(CharSequence s,
                                  int start, int before, int count) {
            if (s.length() == 0) return;

            int number = Integer.valueOf(s.toString());
            if (number > 0 && number <= 10)
                mImage.setImageResource(pickCardId(number));
        }

        @Override
        public void afterTextChanged(Editable s) {   }
    }

    private class OperatorOnClickListener implements View.OnClickListener {
        private String mOp;
        public OperatorOnClickListener(String op) {
            mOp = op;
        }

        @Override
        public void onClick(View v) {
            mStackInputs.push(mOp);
            updateInput();
        }
    }

    private class NumberOnClickListener implements View.OnClickListener {
        private TextView mTextView;
        public NumberOnClickListener(TextView textView) {
            mTextView = textView;
        }

        @Override
        public void onClick(View v) {
            mStackInputs.push(mTextView.getText().toString());
            updateInput();
        }
    }

}
